{-# LANGUAGE TemplateHaskell #-}

module Terminfo.Types
    ( TIDatabase(..)
    , BoolFlags(..)
    ) where

import Terminfo.TH

mkBoolFlags

data TIDatabase = TIDatabase BoolFlags
    deriving (Show)
