{-# LANGUAGE CPP           #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Soap.API where

import Data.Aeson
import qualified Data.Text as T
import Database.Groundhog.Core
import GHC.Generics
import GHC.Int
import Servant.API

import Soap

type API = "soap" :> CRUD Soap
           :<|> "estimate" :> EstimationAPI
           :<|> "color" :> ColorAPI

newtype EKey a = EKey { unEKey :: Int64 }
    deriving (Eq, Show, Ord, Generic)

instance ToJSON (EKey a) where
    toJSON (EKey i) = toJSON i

instance FromJSON (EKey a) where
    parseJSON o = EKey <$> parseJSON o

instance FromHttpApiData (EKey a) where
    parseQueryParam p = EKey <$> parseQueryParam p

instance ToHttpApiData (EKey a) where
    toQueryParam (EKey i) = toQueryParam i

#ifndef ghcjs_HOST_OS
instance PersistField (EKey a) where
    persistName _ = "id"
    toPersistValues (EKey i) = return (const [PersistInt64 i])
    fromPersistValues [PersistInt64 i] = return (EKey i, [])
    fromPersistValues _ = error "impossible case?"
    dbType _ _ = DbTypePrimitive DbInt64 False Nothing Nothing

instance PrimitivePersistField (EKey a) where
    toPrimitivePersistValue   = PersistInt64 . unEKey
    fromPrimitivePersistValue = EKey . fromPrimitivePersistValue
#endif

type CRUD v =
       ReqBody '[JSON] v     :> Post '[JSON] (EKey v)
  :<|> Capture "id" (EKey v) :> Get '[JSON] v
  :<|> Capture "id" (EKey v) :> ReqBody '[JSON] v :> Get '[JSON] NoContent
  :<|> Capture "id" (EKey v) :> Delete '[JSON] NoContent
  :<|> Get '[JSON] [(EKey v, v)]

type EstimationAPI = Capture "id" (EKey Soap)
                                :> QueryParam "shower-length" Double
                                :> QueryParam "budget" Double
                                :> Get '[JSON] (Double,Double,Double)

type ColorAPI = Capture "id" (EKey Soap) :> ReqBody '[JSON] (Int,Int) :> Post '[JSON] T.Text


