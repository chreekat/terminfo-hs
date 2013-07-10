{-# LANGUAGE TemplateHaskell #-}

module Terminfo.Types
    ( TIDatabase(..)
    , BoolCapValues(..)
    , NumCapValues(..)
    ) where

import Terminfo.TH

mkCapValues

data TIDatabase = TIDatabase BoolCapValues NumCapValues
    deriving (Show)
