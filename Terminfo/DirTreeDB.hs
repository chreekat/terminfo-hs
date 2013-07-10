{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Terminfo.DirTreeDB
-- Copyright   :  (c) Bryan Richter (2013)
-- License     :  BSD-style
-- 
-- Maintainer  :  bryan.richter@gmail.com
--
-- An internal module encapsulating methods for parsing a terminfo file as
-- generated by tic(1). The primary reference is the term(5) manpage.

module Terminfo.DirTreeDB
    ( parseDirTreeDB
    ) where

import Development.Placeholders

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, void)
import Data.Attoparsec as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Word (Word16)

import Terminfo.Types
import Terminfo.TH

-- | term(5) defines a short integer as two 8-bit bytes, so:
type ShortInt = Word16

-- | short ints are stored little-endian.
shortInt :: Integral a => ShortInt -> Parser a
shortInt i = word8 first >> word8 second >> (return $ fromIntegral i)
  where
    (second', first') = i `divMod` 256
    second = fromIntegral second'
    first = fromIntegral first'

-- | short ints are stored little-endian.
--
-- (-1) is represented by the two bytes 0o377 0o377.
--
-- Return type is Int so I can include (-1) in the possible outputs. I
-- wonder if I will regret this.
anyShortInt :: Parser Int
anyShortInt = do
    first <- fromIntegral <$> anyWord8
    second <- fromIntegral <$> anyWord8
    return $ if first == 0o377 && second == 0o377
       then (-1)
       else 256*second + first

parseDirTreeDB :: ByteString -> Either String TIDatabase
parseDirTreeDB = parseOnly tiDatabase

tiDatabase :: Parser TIDatabase
tiDatabase = do
    Header{..} <- header
    -- Ignore names
    _ <- A.take namesSize
    bools <- boolCaps boolSize
    -- Align on an even byte
    when (odd boolSize) (void $ A.take 1)
    $(todo "The rest of the parser")
    return $ TIDatabase bools

boolCaps :: Int -> Parser BoolCaps
boolCaps sz = do
    bytes <- B.unpack <$> A.take sz
    let setters = catMaybes $ zipWith trim bytes $mkBoolSetters
    return $ foldl' (flip ($)) ($mkBoolCapsMempty) setters
  where
    trim b f = if b == 1
                  then Just f
                  else Nothing

-- | the magic number for term files
magic :: Parser Int
magic = shortInt 0o432

data Header = Header
     { namesSize :: Int
     , boolSize :: Int
     , numintegers :: Int
     , numOffsets :: Int
     , stringSize :: Int
     }
     deriving (Show)

header :: Parser Header
header = magic >> Header <$> anyShortInt
                         <*> anyShortInt
                         <*> anyShortInt
                         <*> anyShortInt
                         <*> anyShortInt
