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
import Control.Monad ((<=<), filterM)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath

data TIDatabase = TIDatabase

data DBType = BerkeleyDB | DirTreeDB

data BoolTermCap = BoolTermCap
data NumTermCap = NumTermCap
data StrTermCap = StrTermCap

-- Old MacDonald had a farm...
type EIO = EitherT String IO

acquireDatabase
    :: String -- ^ Terminal name
    -> IO (Either String TIDatabase)
       -- ^ A database object for the terminal, if it exists.
acquireDatabase = runEitherT . (parseDBFile <=< findDBFile)

findDBFile :: String -> EIO (DBType, FilePath)
findDBFile term = case term of
    (c:_) -> noteT "No terminfo db found" $ dbFileM c term
    _     -> hoistEither $ Left "User specified null terminal name"

dbFileM c term = dirTreeDB c term <|> berkeleyDB

-- | Not implemented
berkeleyDB = nothing

dirTreeDB :: Char -> String -> MaybeT IO (DBType, FilePath)
dirTreeDB c term = MaybeT $ do
    home <- lookupEnv "HOME"
    path <- findFirst $ dirTreeDBLocs c term home
    return $ (,) DirTreeDB <$> path

dirTreeDBLocs :: Char -> String -> Maybe FilePath -> [FilePath]
dirTreeDBLocs c term home = catMaybes
    [ home <$/> ".terminfo" </> [c] </> term
    , Just $ "/" </> "usr" </> "share" </> "terminfo" </> [c] </> term
    ]

findFirst :: [FilePath] -> IO (Maybe FilePath)
findFirst = fmap headMay . filterM doesFileExist

parseDBFile :: (DBType, FilePath) -> EIO TIDatabase
parseDBFile (db, f) = case db of
    DirTreeDB -> parseDirTreeDB f
    BerkeleyDB -> hoistEither $ Left "BerkeleyDB support not yet implemented"

fa <$/> b = fmap (</> b) fa
infixr 4 <$/>

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
                -> Maybe String
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
