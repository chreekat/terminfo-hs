{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Some phat template haskell for creating some things.
--
-- Problem: there are four(?) bits of code that depend on the list of
-- boolean flags: BoolTermCap data constructors, BoolFlags record
-- accessors, the argument to 'zip' used in parsing the boolean flags
-- (boolSetters), and BoolFlags' mempty expression.
--
-- Rather than specify the flags four separate times, I will use TH to
-- generate the four bits of code from a single canonical list.

module Terminfo.TH
    ( mkBoolFlags
    , mkBoolTermCap
    , mkBoolSetters
    , mkBoolFlagsMempty
    ) where

import Development.Placeholders

import Control.Applicative ((<$>))
import Data.Char (toUpper)
import Data.List (foldl')
import Language.Haskell.TH
import System.IO.Unsafe (unsafePerformIO)

-- |
-- The canonical source
--

theList = unsafePerformIO $ lines <$> readFile "boolFlags"

-- |
-- This splice generates the data definition
--
-- @
--   data BoolFlags = BoolFlags { autoLeftMargin :: Bool, ... }
--       deriving (Show)
-- @
--
-- which is used internally, as part of 'TIDatabase'

mkBoolFlags = fmap (:[]) $ mkBoolFlags' theList

mkBoolFlags' flags =
    dataD (cxt []) (mkName "BoolFlags") [] [dCon flags] [mkName "Show"]

dCon :: [String] -> ConQ
dCon = recC (mkName "BoolFlags") . map mkBoolRec

mkBoolRec :: String -> VarStrictTypeQ
mkBoolRec flag = do
    bool <- [t|Bool|]
    return (mkName flag, NotStrict, bool)

-- |
-- This splice generates the data definition for 'Terminfo.BoolTermCap'.
--
-- > data BoolTermCap = AutoLeftMargin | ...
--

mkBoolTermCap = fmap (:[]) $ mkBoolTermCap' theList

mkBoolTermCap' ls = dataD (cxt []) (mkName "BoolTermCap") [] ctors []
  where
    ctors = map ctor ls
    ctor l = normalC (mkName $ upCase l) []
    upCase (c:cs) = (toUpper c) : cs
    upCase []     = []

-- |
-- This splice generates the expression
--
-- > [ \obj -> obj { autoLeftMargin = True }, ... ]
--
-- which is used in parsing the bool section of terminfo files.
--

mkBoolSetters = mkBoolSetters' theList

mkBoolSetters' ::[String] -> ExpQ
mkBoolSetters' = listE . map mkSetter

mkSetter f = do
    let fName = mkName f
    obj <- newName "obj"
    true <- [|True|]
    let fieldPair = return (fName, true)
    let upd = recUpdE (varE obj) [fieldPair]
    lamE [varP obj] upd

-- |
-- This splice generates the expression
--
-- > BoolFlags False False False ...
--
-- also used for parsing the bool section

mkBoolFlagsMempty = mkBoolFlagsMempty' theList

mkBoolFlagsMempty' =
  foldl' (const . applyToFalse) (conE $ mkName "BoolFlags")

applyToFalse = flip appE [|False|]