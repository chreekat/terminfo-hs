{-# LANGUAGE TemplateHaskell #-}

module System.Terminfo.Types
    ( TIDatabase(..)
    , BoolCapValues(..)
    , NumCapValues(..)
    , StrCapValues(..)
    ) where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

import System.Terminfo.Caps

type BoolCapValues = Map BoolTermCap Bool
type NumCapValues = Map NumTermCap Int
type StrCapValues = Map StrTermCap String

data TIDatabase = TIDatabase BoolCapValues NumCapValues StrCapValues
    deriving (Show)
