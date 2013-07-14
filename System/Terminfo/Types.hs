{-# LANGUAGE TemplateHaskell #-}

module System.Terminfo.Types
    ( TIDatabase(..)
    , BoolCapValues(..)
    , NumCapValues(..)
    , StrCapValues(..)
    ) where

import System.Terminfo.TH

mkCapValues

data TIDatabase = TIDatabase BoolCapValues NumCapValues StrCapValues
    deriving (Show)
