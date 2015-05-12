{-# LANGUAGE TemplateHaskell, GADTs, KindSignatures, FlexibleInstances #-}

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

data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving (Eq,Show)

$(deriveFromJSONTransport defaultOptions{fieldLabelModifier = id, constructorTagModifier = map toLower} ''F)


d :: D Int
d = Record { testOne = 3.14159
           , testTwo = True
           , testThree = Product "test" 'A' 123
           }
