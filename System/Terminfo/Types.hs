{-# LANGUAGE TemplateHaskell #-}

module System.Terminfo.Types
    ( TIDatabase
    , TIDBKey(..)
    , TIDBVal(..)
    ) where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

import System.Terminfo.Caps

data TIDBKey = BoolKey BoolTermCap
             | NumKey NumTermCap
             | StrKey StrTermCap
             deriving (Eq, Ord, Show)

data TIDBVal = BoolVal Bool
             | NumVal Int
             | StrVal String
             deriving (Show)

type TIDatabase = Map TIDBKey TIDBVal
