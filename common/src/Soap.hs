{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes                #-}

module Soap where

import Data.Function
import Data.Ord
import Data.Text
import Data.Aeson
import Data.Foldable
import GHC.Generics
import GHC.Word
#ifndef ghcjs_HOST_OS
import Database.Groundhog
#endif
import Database.Groundhog.TH
import Web.HttpApiData
import Utils

data SoapColors = SoapColors { unSoapColors :: (Text, Text) }
    deriving (Eq, Show, Ord, Generic)

instance ToJSON SoapColors
instance FromJSON SoapColors

data Soap = Soap {
    soapName       :: Text
  , soapDurability :: Double
  , soapImgUrl     :: Text
  , soapColors     :: SoapColors
  } deriving (Eq, Ord, Show, Generic)

mkPersist ghCodeGen [groundhog|
entity: SoapColors
|]

mkPersist ghCodeGen [groundhog|
entity: Soap
|]

instance ToJSON Soap
instance FromJSON Soap

-- | Estimate the optimal L,W,H for a bar of soap given
-- the user's soap preference, shower length, and budgetary constraints
optimalSoapSize
    :: Soap -> Double -> Double -> Double -> (Double, Double, Double)
optimalSoapSize (Soap _ durability _ _) showerLen budget resl = do
    -- print samples
    minimumBy (comparing cost) samples
  where
    grid    = [1, 1 + resl .. 5]  -- One dim sample spacing
    samples = [(l,w, 5/l/w) | l <- grid , w <- grid]

    -- This is the cost function we want to minimize
    cost (l,w,h) =
        let budgetFactor  = (2 - l)^2 + (2 - w)^2*showerLen + (2 - h)^2 / (budget)
            durableFactor = (3 - h)^2 + (2 - w)^2 + (3 - l)^2 / (showerLen + budget)
        in  budgetFactor + durableFactor


tryit :: Double -> Double -> (Double,Double,Double)
tryit l b = optimalSoapSize (Soap undefined 3 undefined undefined) l b 0.25
