{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Some phat template haskell for creating some things.
--
-- Problem: there are four(?) bits of code that depend on the lists of
-- term capabilities: *-TermCap data constructors, *-CapValues record
-- accessors, the argument to 'zip' used in parsing the flags
-- (*-Setters), and *-CapValues' mempty expressions.
--
-- Rather than specify the flags four separate times, I will use TH to
-- generate the four bits of code from a single canonical list. Right now,
-- those canonical lists are kept in standalone files. I may later choose
-- to make one definition the golden standard, and generate the others
-- through reification, but ...

module Terminfo.TH
    ( mkCapValues
    , mkTermCaps
    , mkBoolSetters
    , mkBoolCapsMempty
    ) where

import Development.Placeholders

import Control.Applicative ((<$>))
import Data.Char (toUpper)
import Data.List (foldl')
import Language.Haskell.TH
import System.IO.Unsafe (unsafePerformIO)

--
-- The canonical sources
--

boolList = unsafePerformIO $ lines <$> readFile "boolTermCaps"
numberList = unsafePerformIO $ lines <$> readFile "numberTermCaps"

{-|
This splice generates the data definitions

@
data BoolCaps = BoolCaps { autoLeftMargin :: Bool, ... }
    deriving (Show)

data NumCaps = NumCaps { columns :: Int, ... }
    deriving (Show)
@

which are used internally, as part of 'TIDatabase'
-}

mkCapValues = sequence $
    [ mkCaps' (mkName "BoolCapValues") [t|Bool|] boolList
    , mkCaps' (mkName "NumCapValues") [t|Int|] numberList
    ]

mkCaps' name typ flags =
    dataD (cxt []) name [] [dCon' name typ flags] [mkName "Show"]
  where
    dCon' name typ = recC name . map (mkTypRec typ)

    mkTypRec typ flag = typ >>=
        (\t -> return $ (mkName flag, NotStrict, t))

{- |
This splice generates the data definitions

@
data BoolTermCap = AutoLeftMargin | ...

data NumTermCap = Columns | ...
@

which are part of the public API.
-}

mkTermCaps = sequence $
    [ mkTermCap' "BoolTermCap" boolList
    , mkTermCap' "NumTermCap" numberList
    ]

mkTermCap' name ls = dataD (cxt []) (mkName name) [] ctors []
  where
    ctors = map ctor ls
    ctor l = normalC (mkName $ upCase l) []
    upCase (c:cs) = (toUpper c) : cs
    upCase []     = []

{- |
This splice generates the expression

> [ \obj -> obj { autoLeftMargin = True }, ... ]

which is used in parsing the bool section of terminfo files.
-}


mkBoolSetters = mkBoolSetters' boolList

mkBoolSetters' ::[String] -> ExpQ
mkBoolSetters' = listE . map mkSetter

mkSetter f = do
    let fName = mkName f
    obj <- newName "obj"
    true <- [|True|]
    let fieldPair = return (fName, true)
    let upd = recUpdE (varE obj) [fieldPair]
    lamE [varP obj] upd

{- |
This splice generates the expression

> BoolCaps False False False ...

also used for parsing the bool section
-}

mkBoolCapsMempty = mkBoolCapsMempty' boolList

mkBoolCapsMempty' =
  foldl' (const . applyToFalse) (conE $ mkName "BoolCapValues")

applyToFalse = flip appE [|False|]
