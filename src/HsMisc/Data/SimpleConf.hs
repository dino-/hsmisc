-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- |
   Simple functions for loading config files

   This module was motived by the desire to factor this repetitive 
   configuration file parsing code out of several of my projects.

   These functions offer very simple behavior which may be fine for 
   many tasks. For those needing something that does more, including 
   building and saving config data and .ini-style [section]s, may I 
   suggest Data.ConfigFile 
   <http://hackage.haskell.org/package/ConfigFile>.
-}
module HsMisc.Data.SimpleConf
   ( ConfMap, parseToMap
   , parseToArgs
   )
   where

import Data.Map hiding ( map )
import Data.Maybe ( catMaybes )
import Text.Regex ( matchRegex, mkRegex )


-- | Convenience type synonym. Config data is just a simple Map
type ConfMap = Map String String


{- |
   Parse config file data into a simple (Map String String).

   For example, this:

   >  --- file start ---
   >  foo=one
   >  # a comment
   >
   >  bar
   >  baz-blorp=2
   >  --- file end ---

   becomes:

   >  fromList [("foo","one"),("bar",""),("baz-blorp","2")]

   Comments (prefixed with #) and blank lines in the config file 
   are discarded.
-}
parseToMap :: String -> ConfMap
parseToMap entireConf =
   fromList $ map (\[k, v] -> (k, v))
      $ catMaybes $ map (matchRegex re) $ lines entireConf
   where
      re = mkRegex "^([^#][^=]*)=?(.*)"


{- |
   Parse config file data into what looks like long args on a command 
   line.

   Sometimes it's convenient to be able to supply commonly used 
   long args in a config file. The idea here is you can prepend this 
   [String] to your other command line args and send the whole mess 
   to your System.Console.GetOpt-based code.

   For example, this:

   >  --- file start ---
   >  foo=one
   >  # a comment
   >
   >  bar
   >  baz-blorp=2
   >  --- file end ---

   becomes:

   >  [ "--foo=one", "--bar", "--baz-blorp=2" ]

   As above, comments (prefixed with #) and blank lines in the config 
   file are discarded.
-}
parseToArgs :: String -> [String]
parseToArgs entireConf =
   map prependHyphens $ concat $
      catMaybes $ map (matchRegex re) $ lines entireConf
   where
      re = mkRegex "^([^#][^=]*=?.*)"
      prependHyphens s = '-' : '-' : s
