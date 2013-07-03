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
    , queryBoolCapability
    , queryNumCapability
    , queryStrCapability
    -- ** The Capabalities
    , TermBoolCapability(..)
    , TermNumCapability(..)
    , TermStrCapability(..)
    )
where

import Development.Placeholders

import Data.Text

data TIDatabase = TIDatabase

data TermBoolCapability = TermBoolCapability
data TermNumCapability = TermNumCapability
data TermStrCapability = TermStrCapability

acquireDatabase
    :: Text -- ^ Terminal name
    -> IO (Maybe TIDatabase)
       -- ^ A database object for the terminal, if it exists.
acquireDatabase term = $(todo "acquireDatabase()")

-- | This action wraps both nonexistent and false-valued capabilities into
-- a return value of 'False'.
queryBoolCapability :: TIDatabase
                    -> TermBoolCapability
                    -> IO Bool
queryBoolCapability = $notImplemented

queryNumCapability :: TIDatabase
                   -> TermNumCapability
                   -> IO (Maybe Int)
queryNumCapability = $notImplemented

queryStrCapability :: TIDatabase
                   -> TermStrCapability
                   -> IO (Maybe Text)
queryStrCapability = $notImplemented

-- $queryFuncs I'm not super proud of this interface, but it's the best I
-- can manage at present. For each of these three functions, the first
-- argument is the database to query, and the second argument is the
-- capability to look up.
