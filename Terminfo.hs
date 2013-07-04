{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

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

import Control.Applicative ((<$>), (<|>))
import Control.Error
import Control.Exception
import Control.Monad ((<=<))
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath

data TIDatabase = TIDatabase

data DBType = BerkeleyDB | DirTreeDB

data BoolTermCap = BoolTermCap
data NumTermCap = NumTermCap
data StrTermCap = StrTermCap

-- Old MacDonald had a farm...
type EIO a = EitherT Text IO a

acquireDatabase
    :: Text -- ^ Terminal name
    -> IO (Either Text TIDatabase)
       -- ^ A database object for the terminal, if it exists.
acquireDatabase = runEitherT . (parseDBFile <=< findDBFile)

findDBFile :: Text -> EIO (DBType, FilePath)
findDBFile term =  headET term >>= (\c ->
    (findDirTreeDB c term)
    <|> (findBerkeleyDB term)
    <|> (left "No terminfo file found!"))
  where
    headET = hoistEither . headE "No terminal specified"

headE :: Text -> Text -> Either Text Char
headE e t =
  if (T.length t) > 0
     then Right $ T.head t
     else Left e

tiPath = "/" </> "usr" </> "share" </> "terminfo"

findDirTreeDB c term = EitherT $ do
    let file = tiPath </> [c] </> (T.unpack term)
    e <- doesFileExist file
    return $ if e
       then Right (DirTreeDB, file)
       else Left "No findings"

findBerkeleyDB = const $ hoistEither
    $ Left "BerkeleyDB support not implemented"


parseDBFile :: (DBType, FilePath) -> EIO TIDatabase
parseDBFile (db, f) = case db of
    BerkeleyDB -> hoistEither $ Left "BerkeleyDB support not yet implemented"
    DirTreeDB -> parseDirTreeDB f

parseDirTreeDB :: FilePath -> EIO TIDatabase
parseDirTreeDB = $notImplemented

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
