{-# LANGUAGE TemplateHaskell #-}

module Terminal.Terminfo.Types
    ( TIDatabase(..)
    , BoolCapValues(..)
    , NumCapValues(..)
    , StrCapValues(..)
    ) where

import Terminal.Terminfo.TH

mkCapValues

data TIDatabase = TIDatabase BoolCapValues NumCapValues StrCapValues
    deriving (Show)
