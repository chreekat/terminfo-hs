{-# LANGUAGE TemplateHaskell #-}

module Terminfo.Types
    ( TIDatabase(..)
    , BoolCaps(..)
    ) where

import Terminfo.TH

mkCaps

data TIDatabase = TIDatabase BoolCaps
    deriving (Show)
