{-# LANGUAGE TemplateHaskell #-}

module Terminfo.Types
    ( TIDatabase(..)
    , BoolCapValues(..)
    , NumCapValues(..)
    , StrCapValues(..)
    ) where

import Terminfo.TH

mkCapValues

data TIDatabase = TIDatabase BoolCapValues NumCapValues StrCapValues
    deriving (Show)
