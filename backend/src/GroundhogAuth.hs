{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

{-|

This module allows you to use the auth snaplet with your user database stored
in a groundhog database.  When you run your application with this snaplet, a
config file will be copied into the the @snaplets/groundhog-auth@ directory.
This file contains all of the configurable options for the snaplet and allows
you to change them without recompiling your application.

To use this snaplet in your application enable the session, groundhog, and auth
snaplets as follows:

> data App = App
>     { ... -- your own application state here
>     , _sess :: Snaplet SessionManager
>     , _db   :: Pool Postgresql
>     , _auth :: Snaplet (AuthManager App)
>     }

Then in your initializer you'll have something like this:

> conf <- getSnapletUserConfig
> let cfg = subconfig "postgres" conf
> connstr <- liftIO $ getConnectionString cfg
> ghPool <- liftIO $ withPostgresqlPool (toS connstr) 3 return
> liftIO $ runNoLoggingT (withConn (runDbPersist migrateDB) ghPool)
>
> a <- nestSnaplet "auth" auth $ initGroundhogAuth sess ghPool

If you have not already created the database table for users, it will
automatically be created for you the first time you run your application.

-}

module GroundhogAuth
  ( initGroundhogAuth
  , getConnectionString
  , ghInit
  ) where

------------------------------------------------------------------------------
import           Prelude
import           Control.Applicative
import qualified Control.Exception as E
import           Control.Monad.Trans.Control
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import           Data.Monoid ((<>))
import           Data.Pool
import           Data.Ratio (numerator, denominator)
import           Data.Readable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Database.Groundhog
import           Database.Groundhog.Core
import           Database.Groundhog.Generic
import           Database.Groundhog.Postgresql
import           Database.Groundhog.TH
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session

import           Web.ClientSession (getKey)
--import           Paths_snaplet_groundhog_simple
------------------------------------------------------------------------------


instance NeverNull UserId
instance NeverNull Password

instance PersistField UserId where
    persistName _ = "UserId"
    toPersistValues (UserId bs) = primToPersistValue bs
    fromPersistValues pvs = do
      (a,vs) <- primFromPersistValue pvs
      return (UserId a, vs)
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField UserId where
    toPrimitivePersistValue (UserId a) = toPrimitivePersistValue a
    fromPrimitivePersistValue v = UserId $ fromPrimitivePersistValue v

instance PersistField Password where
    persistName _ = "HashedPassword"
    toPersistValues pw = case pw of
      Encrypted bs -> primToPersistValue $ T.decodeUtf8 bs
      ClearText _  -> error "Attempted to write ClearText password to the database"
    fromPersistValues pvs = do
      (a,vs) <- primFromPersistValue pvs
      return (Encrypted $ T.encodeUtf8 a, vs)
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

mkPersist (defaultCodegenConfig { namingStyle = lowerCaseSuffixNamingStyle })
  [groundhog|
    - entity: AuthUser
      dbName: snap_auth_user
      constructors:
        - name: AuthUser
          fields:
            - name: userId
            - name: userLogin
            - name: userEmail
            - name: userPassword
            - name: userActivatedAt
            - name: userSuspendedAt
            - name: userRememberToken
            - name: userLoginCount
            - name: userFailedLoginCount
            - name: userLockedOutUntil
            - name: userCurrentLoginAt
            - name: userLastLoginAt
            - name: userCurrentLoginIp
            - name: userLastLoginIp
            - name: userCreatedAt
            - name:  userUpdatedAt
            - name: userResetToken
            - name: userResetRequestedAt
            - name: userRoles
              converter: showReadConverter
            - name: userMeta
              converter: showReadConverter
  |]

data GroundhogAuthManager = GroundhogAuthManager
    { gamPool     :: Pool Postgresql
    }

------------------------------------------------------------------------------
-- | Initializer for the groundhog backend to the auth snaplet.
--
initGroundhogAuth
  :: SnapletLens b SessionManager  -- ^ Lens to the session snaplet
  -> Pool Postgresql -- ^ The groundhog snaplet
  -> SnapletInit b (AuthManager b)
initGroundhogAuth sess pool = makeSnaplet "groundhog-auth" desc datadir $ do
    authSettings :: AuthSettings <- authSettingsFromConfig
    key <- liftIO $ getKey (asSiteKey authSettings)
    let manager = GroundhogAuthManager pool
    -- let migrateDB = runMigration $ migrate (undefined :: AuthUser)
    -- liftIO $ runNoLoggingT (withPostgresqlPool (runDbPersist migrateDB) pool)
    -- liftIO $ withPostgresqlPool authSettings pool _
    rng <- liftIO mkRNG
    return $ AuthManager
      { backend = manager
      , session = sess
      , activeUser = Nothing
      , minPasswdLen = asMinPasswdLen authSettings
      , rememberCookieName = asRememberCookieName authSettings
      , rememberCookieDomain = Nothing
      , rememberPeriod = asRememberPeriod authSettings
      , siteKey = key
      , lockout = asLockout authSettings
      , randomNumberGenerator = rng
      }
  where
    desc = "A Groundhog backend for user authentication"
    datadir = Nothing --Just $ liftM (++"/resources/auth") getDataDir


onFailure :: Monad m => E.SomeException -> m (Either AuthFailure a)
onFailure e = return $ Left $ AuthError $ show e

setUid :: DefaultKey AuthUser -> AuthUser -> AuthUser
setUid k u = u { userId = Just $ UserId $ T.pack $ show $ keyToInt k }

runGH
    :: (MonadBaseControl IO m, MonadIO m)
    => Pool Postgresql
    -> Action Postgresql a
    -> m a
runGH pool action = runDbConn action pool

------------------------------------------------------------------------------
-- |
instance IAuthBackend GroundhogAuthManager where
    save GroundhogAuthManager{..} u@AuthUser{..} = do
      case fromText . unUid =<< userId of
        Nothing -> do
          k <- runGH gamPool $ insert u
          return $ Right $ setUid k u
        Just uid -> do
          let go = runGH gamPool $ do
                replace (intToKey uid) u >> return (Right u)
          E.catch go onFailure


    lookupByUserId GroundhogAuthManager{..} uid = do
      case fromText (unUid uid) of
        Nothing -> return Nothing
        Just keyInt -> do
            let key = intToKey keyInt
            mu <- runGH gamPool $ get key
            return $ setUid key <$> mu

    lookupByLogin GroundhogAuthManager{..} login = do
      res <- runGH gamPool $
               project (AutoKeyField, AuthUserConstructor)
                       (UserLoginField ==. login)
      case res of
        [] -> return Nothing
        (k,u):_ -> return $ Just $ setUid k u

    lookupByRememberToken GroundhogAuthManager{..} token = do
      res <- runGH gamPool $
               project (AutoKeyField, AuthUserConstructor)
                       (UserRememberTokenField ==. Just token)
      case res of
        [] -> return Nothing
        (k,u):_ -> return $ Just $ setUid k u

    destroy GroundhogAuthManager{..} AuthUser{..} = do
      runGH gamPool $ delete (UserLoginField ==. userLogin)

pg :: proxy Postgresql
pg = undefined


-------------------------------------------------------------------------------
keyToInt
    :: (PrimitivePersistField (Key a b))
    => Key a b
    -> Int
keyToInt = keyToIntegral


-------------------------------------------------------------------------------
-- | Convert 'Key' to any integral type.
keyToIntegral
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => Key a b
    -> i
keyToIntegral =
    fromPrimitivePersistValue . toPrimitivePersistValue


-------------------------------------------------------------------------------
-- | Type specialized input for type inference convenience.
intToKey
    :: (PrimitivePersistField (Key a b))
    => Int
    -> Key a b
intToKey = integralToKey


-------------------------------------------------------------------------------
-- | Convert any integral type to 'Key'
integralToKey
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => i
    -> Key a b
integralToKey =
    fromPrimitivePersistValue . toPrimitivePersistValue

getConnectionString :: C.Config -> IO ByteString
getConnectionString config = do
    let params =
            [ ["host"]
            , ["hostaddr"]
            , ["port"]
            , ["dbname","db"]
            , ["user"]
            , ["password","pass"]
            , ["connection_timeout"]
            , ["client_encoding"]
            , ["options"]
            , ["application_name"]
            , ["fallback_application_name"]
            , ["keepalives"]
            , ["keepalives_idle"]
            , ["keepalives_interval"]
            , ["keepalives_count"]
            , ["sslmode"]
            , ["sslcompression"]
            , ["sslcert"]
            , ["sslkey"]
            , ["sslrootcert"]
            , ["sslcrl"]
            , ["requirepeer"]
            , ["krbsrvname"]
            , ["gsslib"]
            , ["service"]
            ]
    connstr <- fmap mconcat $ mapM showParam params
    extra   <- fmap TB.fromText $ C.lookupDefault "" config "connectionString"
    return $! T.encodeUtf8 (TL.toStrict (TB.toLazyText (connstr <> extra)))
  where
    qt = TB.singleton '\''
    bs = TB.singleton '\\'
    sp = TB.singleton ' '
    eq = TB.singleton '='

    lookupConfig = foldr (\name names -> do
                            mval <- C.lookup config name
                            case mval of
                              Nothing -> names
                              Just _  -> return mval)
                         (return Nothing)

    showParam [] = undefined
    showParam names@(name:_) = do
      mval :: Maybe C.Value <- lookupConfig names
      let key = TB.fromText name <> eq
      case mval of
        Nothing           -> return mempty
        Just (C.Bool   x) -> return (key <> showBool x <> sp)
        Just (C.String x) -> return (key <> showText x <> sp)
        Just (C.Number x) -> return (key <> showNum  x <> sp)
        Just (C.List   _) -> return mempty

    showBool x = TB.decimal (fromEnum x)

    nd ratio = (numerator ratio, denominator ratio)

    showNum (nd -> (n,1)) = TB.decimal n
    showNum x             = TB.formatRealFloat TB.Fixed Nothing
                             ( fromIntegral (numerator   x)
                             / fromIntegral (denominator x) :: Double )

    showText x = qt <> loop x
      where
        loop (T.break escapeNeeded -> (a,b))
          = TB.fromText a <>
              case T.uncons b of
                Nothing      ->  qt
                Just (c,b')  ->  escapeChar c <> loop b'

    escapeNeeded c = c == '\'' || c == '\\'

    escapeChar c = case c of
                     '\'' -> bs <> qt
                     '\\' -> bs <> bs
                     _    -> TB.singleton c


-- ghInit :: SessionSnapletInit b (Pool Postgresql)
ghInit = makeSnaplet "groundhog" "" Nothing $ do
    cfg     <- getSnapletUserConfig
    connstr <- liftIO $ getConnectionString cfg
    pool <- liftIO $ withPostgresqlPool (unpack connstr) 3 return
    return pool
