{- hpodder component
Copyright (C) 2006-2010 John Goerzen <jgoerzen@complete.org>

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
   Copyright  : Copyright (C) 2006-2010 John Goerzen
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
import Data.String.Utils(strip, split)
import System.Posix.Files(rename, setFileCreationMask)
import System.Log.Logger

getDefaultCP =
    do return $ forceEither $ 
              do cp <- set startingcp "DEFAULT" "serverbase" "https://api.twitter.com"
                 cp <- set cp "DEFAULT" "urlbase" "%(serverbase)s/1.1"
                 cp <- set cp "DEFAULT" "oauthrequesttoken" "%(serverbase)s/oauth/request_token"
                 cp <- set cp "DEFAULT" "oauthaccesstoken" "%(serverbase)s/oauth/access_token"
                 cp <- set cp "DEFAULT" "oauthauthorize" "%(serverbase)s/oauth/authorize"
                 cp <- set cp "DEFAULT" "sendmail" "/usr/sbin/sendmail"
                 cp <- set cp "DEFAULT" "shortenurls" "yes"
                 cp <- set cp "DEFAULT" "shortenall" "no"
                 return cp

startingcp = emptyCP {accessfunc = interpolatingAccess 10}

getCPName =
    do appdir <- getUserDocumentsDirectory
       return $ appdir ++ "/.twidgerc"

loadCP useDefaultIfMissing cpgiven = 
    do cpname <- case cpgiven of
                   Nothing -> getCPName
                   Just x -> return x
       defaultcp <- getDefaultCP
       dfe <- doesFileExist cpname
       debugM "Config" $ "CP " ++ cpname ++ " exists? " ++ show dfe
       if dfe
          then do cp <- readfile defaultcp cpname
                  return $ forceEither cp
          else if useDefaultIfMissing
               then return defaultcp
               else do fail $ "No config file found at " ++ cpname ++ 
                           "\nRun twidge setup to configure twidge for use."

writeCP cpgiven cp =
    do cpname <- case cpgiven of
                   Nothing -> getCPName
                   Just x -> return x
       let tempname = cpname ++ ".write.tmp"
       setFileCreationMask 0o0077
       writeFile tempname (to_string cp)
       rename tempname cpname

getList :: ConfigParser -> String -> String -> Maybe [String]
getList cp sect key = 
       case get cp sect key of
         Right x -> Just (splitit x)
         Left _ -> Nothing
    where splitit x = filter (/= "") . map strip . split "," $ x
  
