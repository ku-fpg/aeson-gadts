{-# LANGUAGE TemplateHaskell, GADTs, KindSignatures, FlexibleInstances, StandaloneDeriving #-}

module Main where

import Transport
import TH

import Data.Aeson
import Data.Aeson.TH
import Data.Char

import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( VarStrictType )

data F :: * -> * where
   F :: Int -> F Int
   G :: Bool -> F Bool
   H :: Int -> Bool -> Float -> [Int] -> F ()

deriving instance Show (F a)
instance Show (Transport F) where
   show (Transport f) = show f

data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving (Eq,Show)

$(deriveToJSON defaultOptions ''F)
$(deriveFromJSONTransport defaultOptions ''F)

d :: D Int
d = Record { testOne = 3.14159
           , testTwo = True
           , testThree = Product "test" 'A' 123
           }
