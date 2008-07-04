{- hpodder component
Copyright (C) 2006-2008 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module     : Config
   Copyright  : Copyright (C) 2006-2008 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}
module Config where

import System.Directory
import Data.ConfigFile
import Control.Monad
import Data.Either.Utils
import System.Path
import Data.String.Utils(strip, split)

getFeedTmp =
    do appdir <- getAppDir
       return $ appdir ++ "/feedxfer"

bracketFeedCWD func =
    do feeddir <- getFeedTmp
       brackettmpdirCWD (feeddir ++ "/tmp-XXXXXX") func

getEnclTmp =
    do appdir <- getAppDir
       return $ appdir ++ "/enclosurexfer"

getAppDir = do appdir <- getAppUserDataDirectory "hpodder"
               return appdir

getDBName = 
    do appdir <- getAppDir
       return $ appdir ++ "/hpodder.db"

getDefaultCP =
    do docsdir <- getUserDocumentsDirectory
       let downloaddir = docsdir ++ "/podcasts"
       return $ forceEither $ 
              do cp <- add_section startingcp "general"
                 cp <- set cp "general" "showintro" "yes"
                 cp <- set cp "DEFAULT" "downloaddir" downloaddir
                 cp <- set cp "DEFAULT" "namingpatt" 
                       "%(safecasttitle)s/%(safefilename)s"
                 cp <- set cp "DEFAULT" "maxthreads" "2"
                 cp <- set cp "DEFAULT" "progressinterval" "1"
                 cp <- set cp "DEFAULT" "podcastfaildays" "21"
                 cp <- set cp "DEFAULT" "podcastfailattempts" "15"
                 cp <- set cp "DEFAULT" "epfaildays" "21"
                 cp <- set cp "DEFAULT" "epfailattempts" "15"
                 cp <- set cp "DEFAULT" "renametypes" "audio/mpeg:.mp3,audio/mp3:.mp3,x-audio/mp3:.mp3"
                 cp <- set cp "DEFAULT" "postproctypes" "audio/mpeg,audio/mp3,x-audio/mp3"
                 cp <- set cp "DEFAULT" "gettypecommand" "file -b -i \"${EPFILENAME}\""
                 cp <- set cp "DEFAULT" "postproccommand" "id3v2 -T \"${EPID}\" -A \"${CASTTITLE}\" -t \"${EPTITLE}\" --WOAF \"${EPURL}\" --WOAS \"${FEEDURL}\" \"${EPFILENAME}\""
                 return cp

startingcp = emptyCP {accessfunc = interpolatingAccess 10}

getCPName =
    do appdir <- getAppDir
       return $ appdir ++ "/hpodder.conf"

loadCP = 
    do cpname <- getCPName
       defaultcp <- getDefaultCP
       dfe <- doesFileExist cpname
       if dfe
          then do cp <- readfile defaultcp cpname
                  return $ forceEither cp
          else return defaultcp

writeCP cp =
    do cpname <- getCPName
       writeFile cpname (to_string cp)

-- FIXME: these may be inefficient [PERFORMANCE]

getMaxThreads :: IO Int
getMaxThreads =
    do cp <- loadCP
       return $ read . forceEither $ get cp "general" "maxthreads"

getProgressInterval :: IO Int
getProgressInterval =
    do cp <- loadCP
       return $ read . forceEither $ get cp "general" "progressinterval"

getList :: ConfigParser -> String -> String -> Maybe [String]
getList cp sect key = 
       case get cp sect key of
         Right x -> Just (splitit x)
         Left _ -> Nothing
    where splitit x = filter (/= "") . map strip . split "," $ x
  