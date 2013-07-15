{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Some phat template haskell for creating some things.
--
-- Problem: there are four(?) bits of code that depend on the lists of term
-- capabilities: *-TermCap data constructors, *-CapValues record accessors,
-- the argument to 'zip' used in parsing the flags (*-Setters), and
-- *-CapValues' mempty expressions.
--
-- Rather than specify the flags four separate times, I will use TH to
-- generate the four bits of code from a single canonical list. Right now,
-- those canonical lists are kept in standalone files. I may later choose
-- to make one definition the golden standard, and generate the others
-- through reification, but ...

module System.Terminfo.TH (
    -- * Type Declarations
    mkTermCaps
    -- * Getters

    -- |
    -- > (\x -> case x of
    -- >     AutoLeftMargin -> autoLeftMargin
    -- >     ...
    -- >     ) :: BoolTermCap -> BoolCapValues -> Bool
    , mkBoolGetter
    , mkNumGetter
    , mkStrGetter
    ) where

import Control.Applicative ((<$>))
import Data.Char (toUpper)
import Language.Haskell.TH
import System.IO.Unsafe (unsafePerformIO)

--
-- The canonical sources
--

boolList = unsafePerformIO $ lines <$> readFile "boolTermCaps"
numberList = unsafePerformIO $ lines <$> readFile "numberTermCaps"
stringList = unsafePerformIO $ lines <$> readFile "stringTermCaps"

{- |
@
data BoolTermCap = AutoLeftMargin | ...

data NumTermCap = Columns | ...

data StrTermCap = BackTab | ...
@
-}

mkTermCaps = sequence
    [ mkTermCap "BoolTermCap" boolList
    , mkTermCap "NumTermCap" numberList
    , mkTermCap "StrTermCap" stringList
    ]

mkTermCap name ls = dataD (cxt []) (mkName name) [] ctors derivings
  where
    ctors = map ctor ls
    ctor l = normalC (mkName $ upCase l) []
    derivings = map mkName ["Ord", "Eq", "Enum", "Bounded", "Show"]

-- Used below too
upCase (c:cs) = toUpper c : cs
upCase []     = []

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
    ctorPats = map (flip ConP [] . mkName . upCase) ls
    getterBodies = map (NormalB . VarE . mkName . ("tc_" ++)) ls
