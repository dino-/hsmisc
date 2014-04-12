-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- |
   Functions to prepend the current time to a string.
   Handy convenience functions for logging.

   Examples:

   > tsL "foo"
   > tsZ "foo"
   > tsLFmt "%B %e, %Y :  " "bar"
   > tsUFmt "%c " "baz"

   you'd get:

   > "2009-06-07 01:53:22 EDT> foo"
   > "2009-06-07T13:43:23Z> foo"
   > "June  7, 2009 :  bar"
   > "Sun Jun  7 13:15:41 UTC 2009 baz"
-}

module HsMisc.Data.TimeStamp
   ( tsLFmt, tsUFmt
   , tsL, tsU, tsZ
   )
   where

import Control.Monad ( liftM )
import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( formatTime )
import Data.Time.LocalTime ( utcToLocalZonedTime )
import System.Locale ( defaultTimeLocale )


{- |
   Prepend the time right now onto the supplied string in the format 
   specified by the format string. Your local time zone.
   For formatting string options see Data.Time.Format
-}
tsLFmt :: String      -- ^ Date format string
       -> String      -- ^ String onto which date will be prepended
       -> IO String
tsLFmt fmt s = do
   timeStamp <- liftM (formatTime defaultTimeLocale fmt)
      $ getCurrentTime >>= utcToLocalZonedTime
   return $ timeStamp ++ s


{- |
   Prepend the time right now onto the supplied string in the format 
   specified by the format string. Universal time.
   For formatting string options see Data.Time.Format
-}
tsUFmt :: String      -- ^ Date format string
       -> String      -- ^ String onto which date will be prepended
       -> IO String
tsUFmt fmt s = do
   timeStamp <- liftM (formatTime defaultTimeLocale fmt)
      getCurrentTime
   return $ timeStamp ++ s


{- |
   Prepend the time right now onto the supplied string. Your time
   zone. Time is in a clear, sortable format like:
   2009-06-04 17:57:13 EDT>
-}
tsL :: String      -- ^ String onto which date will be prepended
    -> IO String
tsL = tsLFmt "%Y-%m-%d %H:%M:%S %Z> "


{- |
   Prepend the time right now onto the supplied string. Universal
   time. Time is in a clear, sortable format like:
   2009-06-04 21:57:13 UTC>
-}
tsU :: String      -- ^ String onto which date will be prepended
    -> IO String
tsU = tsUFmt "%Y-%m-%d %H:%M:%S %Z> "


{- |
   Prepend the time right now onto the supplied string. Universal
   time. Time is in an ISO 8601 format, zero UTC offset or 'Zulu' 
   time: 2009-06-04T21:57:13Z>
-}
tsZ :: String      -- ^ String onto which date will be prepended
    -> IO String
tsZ = tsUFmt "%Y-%m-%dT%H:%M:%SZ> "
