{-# LANGUAGE TemplateHaskell, GADTs, KindSignatures #-}

module Transport where

import Data.Aeson
import Data.Aeson.TH
import Data.Char

import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( VarStrictType )

data Transport :: (* -> *) -> * where
  Transport :: (ToJSON a) => f a -> Transport f

