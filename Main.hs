{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson
import Data.Aeson.TH
import Data.Char

data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving (Eq,Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''D)


d :: D Int
d = Record { testOne = 3.14159
           , testTwo = True
           , testThree = Product "test" 'A' 123
           }
