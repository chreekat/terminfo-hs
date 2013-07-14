{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Terminal.Terminfo.Internal

import Data.Maybe

prop_useOverride ovr =
    forAll notLikely $ \usr ->
    forAll notLikely $ \tds ->
    forAll oftenNull $ \defs ->
    classify (isNothing usr && isNothing tds && null defs)
        "Nothing else specified"$
    classify (isJust usr && isJust tds && not (null defs))
        "Everything else specified"$
    locationsPure (Just ovr) usr tds defs == [ovr]

notLikely = frequency [(2, return Nothing), (1, fmap Just arbitrary)]
oftenNull = frequency [(2, return []), (1, arbitrary)]

prop_includeHome usr =
    forAll notLikely $ \tds ->
    forAll oftenNull $ \defs ->
    let
        withUsr    = locationsPure Nothing (Just usr) tds defs
        withoutUsr = locationsPure Nothing Nothing    tds defs
    in
    classify (isJust tds || not (null defs))
        "Something besides HOME specified"$
    withUsr == usr : withoutUsr

prop_terminfoDirsOverridesDefaults tds =
    forAll (oneof [return [], listOf1 arbitrary]) $ \defs ->
    classify (not $ null defs) "Defaults specified"$
    locationsPure Nothing Nothing (Just tds) defs
        == parseTDVar defs tds

prop_useDefaults defs =
    locationsPure Nothing Nothing Nothing defs == defs

main = $quickCheckAll
