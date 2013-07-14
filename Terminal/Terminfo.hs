{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Terminal.Terminfo
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
--  uglyExample :: IO (Maybe Int)
--  uglyExample = do
--      term \<- fromJust \<$> lookupEnv \"TERM\"
--      db \<- 'acquireDatabase' term
--      let maxColors (Right d) = 'queryNumTermCap' d 'TCN_MaxColors'
--      return $ maxColors db
-- @
--
-- >>> uglyExample
-- Just 256
--

module Terminal.Terminfo (
    -- * Acquiring a Database
      acquireDatabase
    , TIDatabase
    -- * Querying Capabilities
    -- $queryFuncs
    , queryBoolTermCap
    , queryNumTermCap
    , queryStrTermCap
    -- * The Capabilities
    -- $capabilities

    -- ** Boolean
    , BoolTermCap(..)
    -- ** Numeric
    , NumTermCap(..)
    -- ** String
    , StrTermCap(..)
    )
where

import Development.Placeholders

import Control.Applicative ((<$>), (<|>), (<*>), pure)
import Control.Error
import Control.Monad ((<=<), filterM)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import System.Directory
import System.FilePath
import System.IO

import Terminal.Terminfo.Types
import Terminal.Terminfo.DirTreeDB
import Terminal.Terminfo.TH
import Terminal.Terminfo.Internal (terminfoDBLocs)

data DBType = BerkeleyDB | DirTreeDB
    deriving(Show)

mkTermCaps

-- Old MacDonald had a farm...
type EIO = EitherT String IO

acquireDatabase
    :: String -- ^ Terminal name
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
queryBoolTermCap (TIDatabase vals _ _) cap = $mkBoolGetter cap vals

queryNumTermCap :: TIDatabase
                -> NumTermCap
                -> Maybe Int
queryNumTermCap (TIDatabase _ vals _) cap = $mkNumGetter cap vals

-- | As this is a dead simple module, no \'smart\' handling of the
-- returned string is implemented. In particular, placeholders for
-- buffer characters and command arguments are left as-is. This will be
-- rectified eventually, probably in a separate module.
queryStrTermCap :: TIDatabase
                -> StrTermCap
                -> Maybe String
queryStrTermCap (TIDatabase _ _ vals) cap = $mkStrGetter cap vals




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
-- There are no less than 497 capabilities specified in term.h on my
-- Intel-based Ubuntu 12.04 notebook (slightly fewer in the terminfo(5) man
-- page). The naive way of making these available to the user is as data
-- constructors, and that is what I have done here. There are significant
-- drawbacks to this scheme, however.
--
-- The number of constructors absolutely crushes the namespace. I've tried
-- to make this a little nicer by adding the 'TCc_' prefix to each name,
-- but that is a questionable solution. ('c' is one of B, N, or S,
-- representing Bool, Num, and String respectively.)
--
-- Distressingly, GHC 7.6.3 eats up gobs of memory when compiling the
-- module that defines these constructors:
--
-- @
-- <<ghc: 4726807828 bytes, 9003 GCs, 50165760\/140188620 avg\/max bytes
-- residency (17 samples), 400M in use, 0.00 INIT (0.00 elapsed), 20.43 MUT
-- (25.41 elapsed), 27.01 GC (27.71 elapsed) :ghc>>
-- @
--
-- Frankly, this seems a bit buggy, but I haven't looked into it yet. In
-- the meanwhile, it makes compilation very slow!  Especially painful when
-- proofing Haddocks...
--
-- Finally, in order to stay DRY, I'm generating a large number of
-- expressions and data definitions with Template Haskell. I don't find
-- anything wrong with that solution, given the circumstances, but it does
-- make me question the circumstances.
--
-- Here follows the list of capabilities. For descriptions of these,
-- consult terminfo(5).
