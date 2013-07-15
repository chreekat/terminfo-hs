module System.Terminfo.Types
    ( TIDatabase(..)
    , TCBMap(..)
    , TCNMap(..)
    , TCSMap(..)
    ) where

import Data.Map.Lazy (Map)

import System.Terminfo.Caps

data TCBMap = TCBMap (Map BoolTermCap Bool)
    deriving (Show)
data TCNMap = TCNMap (Map NumTermCap Int)
    deriving (Show)
data TCSMap = TCSMap (Map StrTermCap String)
    deriving (Show)

data TIDatabase = TIDatabase TCBMap TCNMap TCSMap
    deriving (Show)
