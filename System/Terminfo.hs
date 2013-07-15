{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  System.Terminfo
-- Copyright   :  (c) Bryan Richter (2013)
-- License     :  BSD-style
-- Maintainer  :  bryan.richter@gmail.com
--
-- This is a pure-Haskell (no FFI) module for accessing terminfo databases,
-- which contain characteristics, or capabilities, for the various
-- terminals such as screen, vt100, or xterm. Among other things, the
-- capabilities include the idiosyncratic character sequences needed to
-- send commands to the terminal. These commands include things like cursor
-- movement.
--
-- For a deeper understanding of terminfo, consult the man pages for
-- term(5) and terminfo(5).
--
-- There are three parts to this module: acquiring a terminfo database,
-- querying the database, and defining the capabilities.
--
-- This module is dead simple, so a single example will hopefully suffice
-- to demonstrate its usage.
--
-- @
-- import System.Terminfo
-- import System.Terminfo.Caps as C
-- uglyExample :: IO (Maybe Int)
-- uglyExample = do
--     term \<- fromJust \<$> lookupEnv \"TERM\"
--     db \<- 'acquireDatabase' term
--     let maxColors (Right d) = 'queryNumTermCap' d C.'MaxColors'
--     return $ maxColors db
-- @
--
-- >>> uglyExample
-- Just 256
--

module System.Terminfo (
    -- * Acquiring a Database
      acquireDatabase
    -- * Querying Capabilities
    -- $queryFuncs
    , queryBoolTermCap
    , queryNumTermCap
    , queryStrTermCap
    -- * The Capabilities
    -- $capabilities

    -- * The Database Type
    , TIDatabase

    ) where

import Control.Applicative ((<$>), (<|>), (<*>), pure)
import Control.Error
import Control.Monad ((<=<), filterM)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.Map.Lazy as M
import System.Directory
import System.FilePath
import System.IO

import System.Terminfo.Types
import System.Terminfo.DirTreeDB
import System.Terminfo.TH
import System.Terminfo.Internal (terminfoDBLocs)
import System.Terminfo.Caps

data DBType = BerkeleyDB | DirTreeDB
    deriving(Show)

-- Old MacDonald had a farm...
type EIO = EitherT String IO

acquireDatabase
    :: String -- ^ System name
    -> IO (Either String TIDatabase)
       -- ^ A database object for the terminal, if it exists.
acquireDatabase = runEitherT . (parseDBFile <=< findDBFile)

findDBFile :: String -> EIO (DBType, FilePath)
findDBFile term = case term of
    (c:_) -> dbFileM c term `orLeft` "No terminfo db found"
    _     -> hoistEither $ Left "User specified null terminal name"
  where
    orLeft = flip noteT

dbFileM c term = dirTreeDB c term <|> berkeleyDB

-- | Not implemented
berkeleyDB = nothing

dirTreeDB :: Char -> String -> MaybeT IO (DBType, FilePath)
dirTreeDB c term = MaybeT $ do
    path <- findFirst =<< map (</> [c] </> term) <$> terminfoDBLocs
    return $ (,) DirTreeDB <$> path

findFirst :: [FilePath] -> IO (Maybe FilePath)
findFirst = fmap headMay . filterM doesFileExist

parseDBFile :: (DBType, FilePath) -> EIO TIDatabase
parseDBFile (db, f) = case db of
    DirTreeDB -> extractDirTreeDB f
    BerkeleyDB -> hoistEither
        $ Left "BerkeleyDB support not yet implemented"

-- | Extract a 'TIDatabase' from the specified file. IO exceptions are
-- left to their own devices.
extractDirTreeDB :: FilePath
                 -> EIO TIDatabase
extractDirTreeDB =
    hoistEither . parseDirTreeDB
    <=< rightT . B.hGetContents
    <=< rightT . flip openBinaryFile ReadMode
  where
    rightT = EitherT . fmap Right

queryBoolTermCap :: TIDatabase
                 -> BoolTermCap
                 -> Bool
queryBoolTermCap (TIDatabase (TCBMap vals) _ _) cap =
    fromMaybe False $ M.lookup cap vals

queryNumTermCap :: TIDatabase
                -> NumTermCap
                -> Maybe Int
queryNumTermCap (TIDatabase _ (TCNMap vals) _) cap = M.lookup cap vals

-- | As this is a dead simple module, no \'smart\' handling of the
-- returned string is implemented. In particular, placeholders for
-- buffer characters and command arguments are left as-is. This will be
-- rectified eventually, probably in a separate module.
queryStrTermCap :: TIDatabase
                -> StrTermCap
                -> Maybe String
queryStrTermCap (TIDatabase _ _ (TCSMap vals)) cap = M.lookup cap vals

--
-- DOCUMENTATION
--



-- $queryFuncs
--
-- For each of these three actions, the first argument is the database to
-- query, and the second argument is the capability to look up.
--
-- I'm not super proud of this interface, but it's the best I can manage at
-- present without requiring lots of mostly-empty case expressions. Perhaps
-- someone will suggest a more interesting solution.

-- $capabilities
--
-- /see/ "System.Terminfo.Caps"
--
-- There are no less than 497 capabilities specified in term.h on my
-- Intel-based Ubuntu 12.04 notebook (slightly fewer in the terminfo(5) man
-- page). The naive way of making these available to the user is as data
-- constructors, and that is what I have done here.
--
-- The number of constructors absolutely crushes the namespace. I have
-- sequestered them into their own module to try to alleviate the pain.
