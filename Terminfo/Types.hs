{-# LANGUAGE TemplateHaskell #-}

module Terminfo.Types
    ( TIDatabase(..)
    , BoolCaps(..)
    , NumCaps(..)
    ) where

import Terminfo.TH

mkCaps

data TIDatabase = TIDatabase BoolCaps {- NumCaps -}
    deriving (Show)
