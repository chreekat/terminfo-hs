{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Terminfo
-- Copyright   :  (c) Bryan Richter (2013)
-- License     :  BSD-style
-- 
-- Maintainer  :  bryan.richter@gmail.com
-- Portability :  portable
--
-- A pure-Haskell (no FFI) module for interacting with terminfo
-- databases.

module Terminfo (
    -- * Acquiring a Database
      acquireDatabase
    , TIDatabase
    -- * Querying Capabilities
    -- $queryFuncs
    , queryBoolTermCap
    , queryNumTermCap
    , queryStrTermCap
    -- ** The Capabilities
    , BoolTermCap(..)
    , NumTermCap(..)
    , StrTermCap(..)
    -- * Rationale
    -- ** TIDatabase
    -- $rationaleTID
    )
where

import Development.Placeholders

import Data.Text

data TIDatabase = TIDatabase

data BoolTermCap = BoolTermCap
data NumTermCap = NumTermCap
data StrTermCap = StrTermCap

acquireDatabase
    :: Text -- ^ Terminal name
    -> IO (Maybe TIDatabase)
       -- ^ A database object for the terminal, if it exists.
acquireDatabase term = $(todo "acquireDatabase()")

-- | This action wraps both nonexistent and false-valued capabilities into
-- a return value of 'False'.
queryBoolTermCap :: TIDatabase
                 -> BoolTermCap
                 -> Bool
queryBoolTermCap = $notImplemented

queryNumTermCap :: TIDatabase
                -> NumTermCap
                -> Maybe Int
queryNumTermCap = $notImplemented

-- | As this is a dead simple module, no 'smart' handling of the returned
-- string is implemented. In particular, placeholders for buffer characters
-- and command arguments are left as-is. This will be rectified eventually,
-- probably in a separate module.
queryStrTermCap :: TIDatabase
                -> StrTermCap
                -> Maybe Text
queryStrTermCap = $notImplemented

-- $queryFuncs
--
-- For each of these three actions, the first argument is the database to
-- query, and the second argument is the capability to look up.
--
-- I'm not super proud of this interface, but it's the best I can manage at
-- present without requiring the user to use lots of mostly-empty case
-- expressions. Perhaps someone will suggest a more interesting solution.

-- $rationaleTID
--
-- One could imagine a simpler interface that hides the TIDatabase type
-- entirely. This could be done naively by rolling 'acquireDatabase' into
-- the query actions, or more \'intelligently\' by creating some
-- 'State'-based monad instance. But the first would read the db file on
-- every query, which strikes me as entirely /too/ naive, and the second
-- would force the user to add Yet Another Monad to their application's
-- stack. Unless somebody has a better idea, I think that exposing
-- TIDatabase strikes the best balance.
