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
    , mkStrSetters
    , mkBoolCapsMempty
    , mkNumCapsMempty
    , mkStrCapsMempty
    , mkBoolGetter
    , mkNumGetter
    , mkStrGetter
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
stringList = unsafePerformIO $ lines <$> readFile "stringTermCaps"

{-|
This splice generates the data definitions

@
data BoolCapValues = BoolCapValues { autoLeftMargin :: Bool, ... }
    deriving (Show)

data NumCapValues = NumCapValues { columns :: Maybe Int, ... }
    deriving (Show)

data StrCapValues = NumCapValues { backTab :: Maybe String, ... }
    deriving (Show)
@

which are used internally, as part of 'TIDatabase'
-}

mkCapValues = sequence $
    [ mkCaps (mkName "BoolCapValues") [t|Bool|] boolList
    , mkCaps (mkName "NumCapValues") [t|Maybe Int|] numberList
    , mkCaps (mkName "StrCapValues") [t|Maybe String|] stringList
    ]

mkCaps name typ flags =
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

data StrTermCap = BackTab | ...
@

which are part of the public API.
-}

mkTermCaps = sequence $
    [ mkTermCap "BoolTermCap" boolList
    , mkTermCap "NumTermCap" numberList
    , mkTermCap "StrTermCap" stringList
    ]

mkTermCap name ls = dataD (cxt []) (mkName name) [] ctors []
  where
    ctors = map ctor ls
    ctor l = normalC (mkName $ upCase l) []

-- Used below too
upCase (c:cs) = (toUpper c) : cs
upCase []     = []

{- |
This splice generates the expression

> [ \val obj -> obj { $(name) = val }, ... ]

which is used in parsing the numbers section of terminfo files.
-}


mkBoolSetters = mkSetters boolList
mkNumSetters = mkSetters numberList
mkStrSetters = mkSetters stringList

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
mkStrCapsMempty = mkMempty "StrCapValues" [|Nothing|] stringList

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

-- |
-- This splice generates the expression
--
-- @
--   (\x -> case x of
--       $(dcon1) -> $(getter1)
--       $(dcon2) -> $(getter2)
--       ...
--       ) :: $(type)TermCap) -> $(type)CapValues -> $(valueType)
--
-- @
--
-- E.g.
--
-- @
--   (\x -> case x of
--       AutoLeftMargin -> autoLeftMargin
--       ...
--       ) :: BoolTermCap -> BoolCapValues -> Bool
-- @
--
mkBoolGetter = mkGetter boolList
mkNumGetter = mkGetter numberList
mkStrGetter = mkGetter stringList

-- LamE [VarP x_0]
--      (CaseE (VarE x_0)
--             [Match (ConP AutoLeftMargin [])
--                    (NormalB (VarE autoLeftMargin)) []])
mkGetter ls = do
    x <- newName "x"
    return $ LamE [VarP x] $
        CaseE (VarE x) $
            zipWith (\p b -> Match p b []) ctorPats getterBodies
  where
    ctorPats = map ((flip ConP []) . mkName . upCase) ls
    getterBodies = map (NormalB . VarE . mkName) ls
