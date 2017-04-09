{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Bool
import           Data.Functor.Compose
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.FileEmbed
import           GHCJS.DOM.EventM           (EventM, event, on)
import qualified GHCJS.DOM.HTMLImageElement as ImageElement
import           GHCJS.DOM.MouseEvent       (getClientX, getClientY)
import qualified GHCJS.DOM.Types            as GHCJS
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex
import           Text.Read
import qualified URI.ByteString             as URI
#ifdef ghcjs_HOST_OS
import           GHCJS.DOM.ClientRect       (getHeight, getLeft, getTop,
                                             getWidth)
import           GHCJS.DOM.Element          (click, getBoundingClientRect,
                                             getClientLeft, getClientTop)
#endif
import Text.Printf

import           Soap
import           Soap.API


------------------------------------------------------------------------------
main :: IO ()
main = mainWidgetWithCss $(embedFile "../static/style.css") mainApp


------------------------------------------------------------------------------
type SupportsSoap t m = (DomBuilder t m, HasWebView (Performable m)
                        ,DomBuilderSpace m ~ GhcjsDomSpace
                        ,TriggerEvent t m, PerformEvent t m
                        ,MonadIO (Performable m), PostBuild t m
                        ,MonadHold t m
                        ,MonadFix m
                        ,GHCJS.IsGObject (RawElement (DomBuilderSpace m)))


------------------------------------------------------------------------------
mainApp :: forall t m. SupportsSoap t m => m ()
mainApp = divClass "content" $ do

    rec (soaps, soapSel) <- divClass "left-half" $ soapCrudWidget colorUps

        colorUps <- divClass "right-half" $ do
            cUps <- colorWidget soapSel soaps
            estimationWidget soapSel soaps
            return cUps

    return ()


------------------------------------------------------------------------------
estimationWidget
    :: forall t m. SupportsSoap t m
    => Dynamic t (EKey Soap)
    -> Dynamic t (M.Map (EKey Soap) Soap)
    -> m ()
estimationWidget oid soaps = do

    let (_ :<|> cEstimate :<|> _) = client (Proxy :: Proxy API)
            (Proxy :: Proxy m) (Proxy :: Proxy ()) baseUrl

    let riAttrs = def {_rangeInputConfig_attributes =
                       constDyn $ "min" =: "0.1"  <>
                                  "max" =: "1"    <>
                                  "step" =: "0.01"
                      }

    let dbSoap = M.lookup <$> oid <*> soaps

    void $ dyn $ flip fmap dbSoap $ \case
        Nothing -> blank
        Just oid' -> do
            showerLen <- divClass "range" $ do
                el "label" (text "Shower Length")
                _rangeInput_value <$> rangeInput riAttrs

            budget <- divClass "range" $ do
                el "label" (text "Budget")
                _rangeInput_value <$> rangeInput riAttrs

            estimateTrigger <- debounce 1
                               (() <$ updated ((,) <$> showerLen <*> budget))

            x <- cEstimate (Right <$> oid)
                           (QParamSome . realToFrac <$> showerLen)
                           (QParamSome . realToFrac <$> budget)
                           estimateTrigger

            let defSoap = fromMaybe (Soap undefined 1 undefined undefined)
                          <$> liftA2 M.lookup oid soaps

            let r' = optimalSoapSize
                     <$> defSoap
                     <*> fmap realToFrac showerLen
                     <*> fmap realToFrac budget
                     <*> pure 0.1

            rText :: Dynamic t T.Text <-holdDyn "" $ leftmost
                [("Estimate: " <>) . showSize <$> updated r'
                ,("Exact:    " <>) . showSize <$> fmapMaybe reqSuccess x]
            el "pre" $ dynText rText

            return ()

showDbl :: Double -> T.Text
showDbl d = T.pack $ printf "%.2f" d

showSize :: (Double,Double,Double) -> T.Text
showSize (l,w,h) = T.pack $ printf "Len: %.2f  Wid: %.2f  Hei: %.2f" l w h

------------------------------------------------------------------------------
soapCrudWidget
    :: forall t m. SupportsSoap t m
    => Event t (EKey Soap, Soap)
    -> m (Dynamic t (M.Map (EKey Soap) Soap), Dynamic t (EKey Soap))
soapCrudWidget colorUps = do
    pb <- getPostBuild
    let ((cCr :<|> cRe :<|> cUp :<|> cDel :<|> cReAll) :<|> _ :<|> _) =
            client (Proxy :: Proxy API) (Proxy :: Proxy m) (Proxy :: Proxy ()) baseUrl

    rec soaps <- foldDyn ($) mempty $ leftmost
            [ const . M.fromList <$> soapRefresh
            , ffor soapInstrs $ \case
                    (k, CrudDelete) -> M.delete k
                    (k, CrudCreate s) -> M.insert k s
                    (k, CrudUpdate s) -> M.insert k s
                    (_, CrudSelect)   -> id
            ]

        soapRefresh <- fmapMaybe reqSuccess <$> cReAll refresh
        (listInstrs, newSoap) <- divClass "table" $ do
            l <- selectViewListWithKey selectedSoap soaps soapRow
            n <- soapNewRow
            return (l, n)

        let soapInstrs = leftmost [listInstrs
                                  ,ffor colorUps (\(k,v) -> (k, CrudUpdate v))]


        selectedSoap <- holdDyn (EKey 0)
                        (fst <$> ffilter ((is _CrudSelect) . snd) soapInstrs)

        let refresh = leftmost [pb , () <$ soapResp]

        crArg <- fmap (note "No new soap")
                 <$> holdDyn Nothing ((^? _CrudCreate) <$> newSoap)
        soapResp <- cCr crArg (() <$ updated crArg)

        delArg <- fmap Right <$> holdDyn (EKey 0)
                  (fmap fst . ffilter ((is _CrudDelete) . snd) $ soapInstrs)
        delResp <- cDel delArg (() <$ updated delArg)

        updArg :: Dynamic t (Maybe (EKey Soap, Soap)) <- holdDyn Nothing
            ((\i -> fmap (fst i,) (snd i & (^? _CrudUpdate))) <$> soapInstrs)
        let updKey  = maybe (Left "No key")  (Right . fst) <$> updArg
            updSoap = maybe (Left "No Soap") (Right . snd) <$> updArg

        updRes <- cUp (updKey ) updSoap (() <$ updated updArg)

    return (soaps, selectedSoap)


------------------------------------------------------------------------------
soapRow :: forall t m. SupportsSoap t m
    => EKey Soap
    -> Dynamic t Soap
    -> Dynamic t Bool
    -> m (Event t Instr)
soapRow k v isSel = do
    let cls  = ("class" =:) . ("soap-row" <> ) . bool "" " selected" <$> isSel
        styl = ("style" =:) . soapGradient <$> v
        attr = (<>) <$> cls <*> styl
    (rEl, delClick) <- elDynAttr' "div" attr $ do
        divClass "row-id" $ text $ T.pack $ show $ unEKey k
        divClass "row-name" $ dynText (soapName <$> v)
        divClass "row-dura" $ dynText (T.pack . show . soapDurability <$> v)
        divClass "row-url" $ dynText $ soapImgUrl <$> v
        divClass "row-del" $ button "x"
    return $ leftmost [ CrudDelete <$ delClick
                      , CrudSelect <$ domEvent Click rEl
                      ]

------------------------------------------------------------------------------
soapGradient :: Soap -> T.Text
soapGradient (Soap _ _ _ (SoapColors (c1,c2))) =
    T.concat ["border-image: linear-gradient(to bottom, "
             ,c1',", ", c2', ") 1 100%; border-width: 10px;"]
    where c1' = if T.null c1 then "white" else c1
          c2' = if T.null c2 then "white" else c2


------------------------------------------------------------------------------
soapNewRow :: forall t m. SupportsSoap t m => m (Event t Instr)
soapNewRow = do

    rec isActive <- holdDyn False (leftmost [ True <$ starts
                                               , False <$ ends])
        starts <- displayWhen (not <$> isActive)
            "div" (constDyn ("class" =: "soap-row")) (button "+")

        adds  <- displayWhen isActive "div" (constDyn ("class" =: "soap-row")) $ do
            elAttr "div" ("class" =: "row-id") $ blank

            rec nm  <- elAttr "div" ("class" =: "row-name") $
                       validInput "soap-name" validateName bumps unbumps
                dr  <- elAttr "div" ("class" =: "row-dura") $
                       validInput "soap-dura" validateDura bumps unbumps
                url <- elAttr "div" ("class" =: "row-url")  $
                       validInput "soap-url" validateUrl bumps unbumps

                let soap' = getCompose $ Soap <$> nm <*> dr <*> url <*> pure defCol
                go  <- tagPromptlyDyn soap' <$>
                       elAttr "div" ("class" =: "now-del") (button "+")

                let bumps   = () <$ fmapMaybe (^? _Left ) go
                    unbumps = () <$ fmapMaybe (^? _Right) go

            return $ fmapMaybe (^? _Right) go

        let ends = () <$ adds

    return $ CrudCreate <$> adds

  where
      validateName t
         | T.null t        = Left "Soap must have an name"
         | T.length t > 20 = Left "Too wordy"
         | otherwise       = Right t

      validateDura t = case readMaybe (T.unpack t) of
               Nothing -> Left "Number"
               Just x  | x < 0 -> Left "Durability must be positive"
               Just x  -> Right x

      uriParse = URI.parseURI URI.laxURIParserOptions . TE.encodeUtf8
      validateUrl t = case uriParse t of
               Left  _ -> Left "Bad URL?"
               Right _ -> Right t


------------------------------------------------------------------------------
colorWidget
    :: forall t m. SupportsSoap t m
    => Dynamic t (EKey Soap)
    -> Dynamic t (M.Map (EKey Soap) Soap)
    -> m (Event t (EKey Soap, Soap))
colorWidget oid soaps = do
    let (_ :<|> _ :<|> cColor) =
            client (Proxy :: Proxy API)
                   (Proxy :: Proxy m)
                   (Proxy :: Proxy (EKey Soap, Soap)) -- Tag Reqs with soap key & soap
                   baseUrl
        selSoap  = M.lookup <$> oid <*> soaps
        selSoapK = (\k mv -> fmap (k,) mv) <$> oid <*> selSoap
        imgSrc   = fmap soapImgUrl <$> selSoap

    img <- displayWhen (isJust <$> selSoap) "div" (constDyn ("class" =: "color-div"))$ do
        fmap fst $
            elDynAttr' "img" (("src" =:) . fromMaybe "" <$> imgSrc) blank

#ifdef ghcjs_HOST_OS
    let htmlImg = ImageElement.castToHTMLImageElement $ _element_raw img

    geom <- holdDyn (1 :: Int) =<<
        performEvent (ffor (domEvent Load img) $ \_ -> liftIO $
                             ImageElement.getNaturalWidth htmlImg
                     )

    clicks <- relativizeEvent htmlImg click
    clickPos <- holdDyn (1,1) clicks
#else
    let geom     = constDyn (1 :: Int) :: Dynamic t Int
        clickPos = constDyn (1 :: Int, 1 :: Int) :: Dynamic t (Int,Int)
#endif

    clkColor <- cColor (Right <$> oid) (Right <$> clickPos)
                       (fmapMaybe id $
                        tag (current selSoapK) (domEvent Click img))

    colorPosToOverwrite <- foldDyn (const moveTarget) TargFirst (() <$ clkColor)

    let soap' = attachPromptlyDynWith soapSetColor colorPosToOverwrite clkColor
    return $ traceEvent "soap': " $ fmapMaybe id soap'


data ColorTarget = TargFirst | TargSecond
    deriving (Eq)

moveTarget TargFirst  = TargSecond
moveTarget TargSecond = TargFirst


------------------------------------------------------------------------------
soapSetColor
    :: ColorTarget
    -> ReqResult (EKey Soap, Soap) T.Text
    -> Maybe (EKey Soap, Soap)
soapSetColor targ (ResponseSuccess (k, s@(Soap _ _ _ (SoapColors cs))) c _)
    | targ == TargFirst = Just $ (k, s { soapColors = SoapColors (c, snd cs) })
    | targ /= TargFirst = Just $ (k, s { soapColors = SoapColors (fst cs, c) })
soapSetColor _ _ = Nothing



defCol :: SoapColors
defCol = SoapColors ("white", "grey")

data Instr = CrudDelete
           | CrudCreate Soap
           | CrudUpdate Soap
           | CrudSelect
           deriving (Eq, Ord, Show)


-- makePrisms ''Instr
_CrudDelete :: Prism' Instr ()
_CrudDelete = prism (const CrudDelete) $ \a -> case a of
    CrudDelete -> Right ()
    _          -> Left a

_CrudCreate :: Prism' Instr Soap
_CrudCreate = prism CrudCreate $ \a -> case a of
    CrudCreate s -> Right s
    _            -> Left a

_CrudUpdate :: Prism' Instr Soap
_CrudUpdate = prism CrudUpdate $ \a -> case a of
    CrudUpdate s -> Right s
    _            -> Left a

_CrudSelect :: Prism' Instr ()
_CrudSelect = prism (const CrudSelect) $ \a -> case a of
    CrudSelect -> Right ()
    _          -> Left a


hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _         = Nothing

note :: e -> Maybe a -> Either e a
note e Nothing  = Left e
note _ (Just a) = Right a

#ifdef ghcjs_HOST_OS
baseUrl :: Reflex t => Dynamic t BaseUrl
baseUrl = constDyn $ BasePath "api"
#else
baseUrl :: Reflex t => Dynamic t BaseUrl
baseUrl = constDyn $ BaseFullUrl Http "localhost" 8001 ""
#endif


type VDynamic t a = Compose (Dynamic t) (Either T.Text) a

------------------------------------------------------------------------------
validInput
    :: SupportsSoap t m
    => T.Text
    -> (T.Text -> Either T.Text a)
    -> Event t ()
    -> Event t ()
    -> m (VDynamic t a)
validInput fieldName validate bump unbump = divClass "validating-input" $ do

    rec ti <- elAttr "label" ("for" =: fieldName) $
              textInput $ def & textInputConfig_attributes .~ inputAttrs

        divClass "error-msg" $ dynText errorMsg

        haveBeenBumped <- holdDyn False $ leftmost [True  <$ bump
                                                   ,False <$ unbump]
        let errorState = (\bmp txt -> if   bmp
                                      then (validate txt ^? _Left)
                                      else Nothing
                         ) <$> haveBeenBumped <*> value ti
            errorMsg   = fromMaybe " " <$> errorState
            inputAttrs = ((bool mempty ("size" =: "5")
                           (fieldName == "soap-dura")) <>) .
                         bool mempty ("class" =: "error") .
                         isJust <$> errorState

    return $ Compose $ validate <$> value ti



------------------------------------------------------------------------------
displayWhen
    :: (DomBuilder t m, PostBuild t m)
    => Dynamic t Bool
    -> T.Text
    -> Dynamic t (M.Map T.Text T.Text)
    -> m a
    -> m a
displayWhen b tag attrs ma = do
            let hideAttrs = bool ("style" =: "display:none") mempty <$> b
            elDynAttr tag ((<>) <$> hideAttrs <*> attrs) ma

#ifdef ghcjs_HOST_OS
relativizeEvent e eventName = do
    wrapDomEvent e (`on` eventName) $ do
        ev   <- event
        br   <- fromMaybe (error "Unlikely")
                <$> liftIO (getBoundingClientRect e)
        top  <- liftIO $ getTop br
        left <- liftIO $ getLeft br
        x' <- (\x -> x - floor left) <$> getClientX ev
        y' <- (\y -> y - floor top)  <$> getClientY ev
        return (x', y')
#endif

header :: SupportsSoap t m => m ()
header = do
    elAttr "link" ("href" =: "style.css" <>
                   "rel" =: "stylesheet" <>
                   "type" =: "text/css") blank
