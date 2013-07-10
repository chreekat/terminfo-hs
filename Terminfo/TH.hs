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
-- generate the four bits of code from a single canonical list. Right
-- now, those canonical lists are kept in standalone files. I may later
-- choose to make one definition the golden standard, and generate the
-- others through reification, but ...

module Terminfo.TH
    ( mkCapValues
    , mkTermCaps
    , mkBoolSetters
    , mkNumSetters
    , mkBoolCapsMempty
    , mkNumCapsMempty
    ) where

import Development.Placeholders

import Control.Applicative ((<$>))
import Data.Char (toUpper)
import Data.List (foldl')
import Data.Maybe (fromJust)
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
    , mkCaps' (mkName "NumCapValues") [t|Maybe Int|] numberList
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
    [ mkTermCap "BoolTermCap" boolList
    , mkTermCap "NumTermCap" numberList
    ]

mkTermCap name ls = dataD (cxt []) (mkName name) [] ctors []
  where
    ctors = map ctor ls
    ctor l = normalC (mkName $ upCase l) []
    upCase (c:cs) = (toUpper c) : cs
    upCase []     = []

{- |
This splice generates the expression

> [ \val obj -> obj { $(name) = val }, ... ]

which is used in parsing the numbers section of terminfo files.
-}


mkBoolSetters = mkSetters boolList
mkNumSetters = mkSetters numberList

mkSetters ::[String] -> ExpQ
mkSetters = listE . map mkSetter

-- TODO: lens cleanup candidate
mkSetter cap = do
    -- Sadly, I can't seem to use splicing inside a quasiquoted rec
    -- update. Syntax error.
    obj <- newName "obj"
    val <- newName "val"
    fName <- fromJust <$> lookupValueName cap
    let fieldPair = (fName, VarE val)
        upd = RecUpdE (VarE obj) [fieldPair]
    return $ LamE [VarP val, VarP obj] upd

{- |
This splice generates the expression

> $(name) $(mempt) $(mempt) ...

e.g.

> BoolCapValues False False False ...

also used for parsing the bool section
-}

mkBoolCapsMempty = mkMempty "BoolCapValues" [|False|] boolList
mkNumCapsMempty = mkMempty "NumCapValues" [|Nothing|] numberList

-- At each point in the list, we apply the accumulator (which is a
-- partially-applied data constructor) to the new element. At the end, we
-- should have a fully-applied (kind *) value of type name!
--
-- Note that the contents of the list are immaterial - hence the use of
-- const.
mkMempty :: String -> ExpQ -> [String] -> ExpQ
mkMempty name mempt =
  foldl' (const . applyToMempt)
    (conE =<< fromJust <$> lookupValueName name)

  where
    applyToMempt = flip appE mempt
