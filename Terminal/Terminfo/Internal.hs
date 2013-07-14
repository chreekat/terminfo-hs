module Terminal.Terminfo.Internal where

import Control.Applicative ((<$>), (<|>), (<*>), pure)
import Control.Error
import Control.Monad
import Data.Maybe
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath

terminfoDBLocs :: IO [FilePath]
terminfoDBLocs = locationsPure
    <$> lookupEnv "TERMINFO"
    <*> (lookupEnv "HOME" <$$/> ".terminfo")
    <*> lookupEnv "TERMINFO_DIRS"
    <*> pure ["/lib/terminfo", "/usr/share/terminfo"]


fa <$/> b = (</> b) <$> fa
infixr 4 <$/>

ffa <$$/> b = fmap (<$/> b) ffa
infixr 4 <$$/>

-- | I hate this name. There is undoubtedly a better way of structuring
-- all of this, starting way up at 'dirTreeDB'.
locationsPure :: (Maybe FilePath) -- ^ Override directory
              -> (Maybe FilePath) -- ^ $HOME
              -> (Maybe String)   -- ^ $TERMINFO_DIRS
              -> [FilePath]       -- ^ Defaults
              -> [FilePath]
locationsPure ovr usr termdirs defs = case ovr of
    Just override -> [override]
    Nothing       -> (catMaybes [usr]) ++ system

  where
    system = case termdirs of
        Just list -> parseTDVar defs list
        Nothing   -> defs

parseTDVar defs = (replace "" defs) . (split ':')

-- | Replace an element with multiple replacements
replace :: Eq a => a -> [a] -> [a] -> [a]
replace old news = foldr (\x acc -> if x == old
                                       then news ++ acc
                                       else x : acc)
                         []

-- | split, as seen in ByteString and Text, but for Strings.
split s = foldr go [[]]
  where
    go c acc = if c /= s
                  then unshift c acc
                  else "" : acc

    -- | /perldoc -f unshift/
    unshift c (xs:xss) = (c:xs) : xss
    unshift c []       = [[c]]
