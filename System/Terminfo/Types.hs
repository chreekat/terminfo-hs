{-# LANGUAGE TemplateHaskell #-}

module System.Terminfo.Types
    ( TIDatabase
    ) where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

import System.Terminfo.Caps

data TIDBKey = BoolKey BoolTermCap
             | NumKey NumTermCap
             | StrKey StrTermCap
             deriving (Eq, Ord)

data TIDBVal = BoolVal Bool
             | NumVal Int
             | StrVal String

type TIDatabase = Map TIDBKey TIDBVal
