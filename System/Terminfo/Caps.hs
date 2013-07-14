{-# LANGUAGE TemplateHaskell #-}

-- | There is an egregious number of capabilities. They are sequestered in
-- this module to preserve namespace sanity.
--
-- For descriptions of these, consult terminfo(5).

module System.Terminfo.Caps (
    -- * Boolean
      BoolTermCap(..)
    -- * Numeric
    , NumTermCap(..)
    -- * String
    , StrTermCap(..)
    ) where

import System.Terminfo.TH

mkTermCaps
