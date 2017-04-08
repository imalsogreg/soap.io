{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Soap.Server where

import Control.Arrow (first, left)
import Control.Error
import Control.Monad.Reader
import Data.Complex
import Data.Monoid
import           Control.Monad.Trans.Control
import           Control.Monad.Logger
import           Control.Monad.Trans
import Control.Lens
import qualified Codec.Picture as J
import Data.Colour.RGBSpace.HSL
import Data.Fixed
import Data.Colour.RGBSpace
import qualified Network.Http.Client as C
import Servant.API
import Data.Aeson
import qualified Data.Text as T
import Data.Pool
import Data.Proxy
import qualified Data.Text.Encoding as TE
import GHC.Word
import Snap
import Snap.Core
import Snap.Http.Server
import Snap.Snaplet
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
import Servant.Server
import qualified URI.ByteString as U

import qualified Database.Groundhog.Postgresql as G
import qualified Database.Groundhog.Core as G

import GroundhogAuth
import Soap
import Soap.API

data App = App {
    _gh   :: Snaplet (Pool G.Postgresql)
 ,  _sess :: Snaplet SessionManager
 }
makeLenses ''App

main :: IO ()
main = serveSnaplet defaultConfig app


app :: SnapletInit App App
app = makeSnaplet "app" "Example application" Nothing $ do
    s  <- nestSnaplet "sess" sess $
          initCookieSessionManager "site_key.txt" "sess"
          Nothing (Just 3600)

    g  <- nestSnaplet "groundhog" gh $ ghInit
    let pool = g ^. snapletValue
    liftIO $ (G.runDbConn . G.runMigration $ G.migrate (undefined :: Soap)) pool

    addRoutes [("api", serveSnap (Proxy @API) serveAPI)
              ,("", serveDirectory "static")
              ]

    return $ App g s


type AppHandler = Handler App App

serveAPI :: Server API AppHandler
serveAPI = crudServer :<|> estimationHandler :<|> serveColor


class (G.PrimitivePersistField (G.Key v G.BackendSpecific)) => HasKey v where
    toEKey     :: Key v G.BackendSpecific -> EKey v
    fromEKey   :: EKey v -> Key v G.BackendSpecific
    autoToEKey :: G.AutoKey v -> EKey v

instance HasKey Soap where
    toEKey (SoapKey (G.PersistInt64 i)) = EKey $ i
    fromEKey (EKey i) = SoapKey $ G.PersistInt64 i
    autoToEKey = toEKey


crudServer
    :: (ToJSON a, FromJSON a, G.PersistEntity a, HasKey a)
    => Server (CRUD a) AppHandler
crudServer = c :<|> r :<|> u :<|> d :<|> rAll
  where c a   = autoToEKey <$> runGh (G.insert a)
        r     = noteH err404 . runGh . G.get . fromEKey
        u k a = fmap (const NoContent) $ runGh $ G.replace (fromEKey k) a
        d k   = fmap (const NoContent) $ runGh $ G.deleteBy (fromEKey k)
        rAll  = fmap (first autoToEKey) <$> runGh G.selectAll


estimationHandler
    :: EKey Soap
    -> Maybe Double
    -> Maybe Double
    -> AppHandler (Double, Double, Double)
estimationHandler oid showerLen budget = do
    -- soap <- return $ Soap "ExampleSoap" 5 "example.com/image.jpg" (SoapColors ("black", "black"))
    soap <- noteH err404 . runGh . G.get $ fromEKey oid
    liftIO $ print showerLen
    liftIO $ print budget
    liftIO $ optimalSoapSize soap (fromMaybe 1 showerLen) (fromMaybe 1 budget) 1


noteH :: ServantErr -> AppHandler (Maybe a) -> AppHandler a
noteH e ma = do
    ma >>= \case
        Nothing -> throwError e
        Just r  -> return r


runGh :: G.Action G.Postgresql a -> AppHandler a
runGh a = with gh $ do
    conn <- getsSnapletState (^. snapletValue)
    G.runDbConn a conn

serveColor
    :: EKey Soap
    -> (Int,Int)
    -> AppHandler T.Text
serveColor oid (x,y) = do
    liftIO $ print "serveColorHandler"
    soap <- noteH err404 . runGh . G.get $ fromEKey oid
    liftIO $ print "soap ok"
    img  <- liftIO $ fetchImage (soapImgUrl soap)
    case img of
        Left e -> throwError err500 { errBody = "Bad image fetch" }
        Right img'' -> do
            img' <- return $ J.convertRGB8 img''
            -- liftIO $ putStrLn "Got converted image"
            -- liftIO $ putStrLn $ "fetching color for point: " ++ show (x,y)
            -- liftIO $ J.saveJpgImage 9 "/Users/greghale/test.jpg" (J.ImageRGB8 img')
            -- liftIO $ J.savePngImage "/Users/greghale/test.png" (J.ImageRGB8 img')
            mc   <- return $ getMeanColor img'
            -- liftIO $ putStrLn $ "Got mean color: " ++ show mc
            return mc
  where
       getMeanColor i = hslMean $ mconcat [toMeanPixel (J.pixelAt i (x+x') (y+y')) | x' <- [-5..5], y' <- [-5..5]]

data MeanPixel = MeanPixel { meanPixVals :: ([Double], [Double], [Double]) }
    deriving (Show)

linMean :: [Double] -> Double
linMean xs = sum xs / fromIntegral (length xs)

circMean :: [Double] -> Double
circMean xs = let cs = map (cis . (* (pi / 180))) xs
              in  phase (sum cs) * 180 / pi

rgbMean :: MeanPixel -> T.Text
rgbMean m@(MeanPixel (r,g,b)) = T.concat ["rgb(", t (linMean r), " , ", t (linMean g), ", ", t (linMean b), ")"]
  where t = T.pack . show . round


hslMean :: MeanPixel -> T.Text
hslMean (MeanPixel (rs,gs,bs)) =
    let rgbs = zipWith3 RGB rs gs bs
        (hs,ss,ls) = unzip3 $ hslView <$> rgbs
        (h,s,l) = (circMean hs, linMean ss, linMean ls)
        RGB r g b = hsl h s l
        t = T.pack . show . round
    in T.concat ["rgb( ", t r, " , ", t g, " ,",  t b," )"]


instance Monoid MeanPixel where
    mempty = MeanPixel ([], [], [])
    MeanPixel (r, g, b) `mappend` MeanPixel (r', g', b') = MeanPixel (r <> r', g <> g', b <> b')


toMeanPixel :: J.PixelRGB8 -> MeanPixel
toMeanPixel (J.PixelRGB8 r g b) = MeanPixel ([fromIntegral r], [fromIntegral g], [fromIntegral b])


fetchImage :: T.Text -> IO (Either T.Text J.DynamicImage)
fetchImage url = fmapL (T.pack . show) . J.decodeImage <$>
                 (C.get (TE.encodeUtf8 url) C.concatHandler)

