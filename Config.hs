{- hspod component
Copyright (C) 2006 John Goerzen <jgoerzen@complete.org>

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
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}
module Config where

import System.Directory
import MissingH.ConfigParser
import Control.Monad
import MissingH.Either

getAppDir = do appdir <- getAppUserDataDirectory "hspod"
               dde <- doesDirectoryExist appdir
               unless (dde) (createDirectory appdir)
               return appdir

getDBName = 
    do appdir <- getAppDir
       return $ appdir ++ "/hspod.db"

getDefaultCP =
    do docsdir <- getUserDocumentsDirectory
       let downloaddir = docsdir ++ "/podcasts"
       return $ forceEither $ 
              do cp <- add_section startingcp "general"
                 cp <- set cp "general" "showintro" "yes"
                 cp <- set cp "DEFAULT" "downloaddir" downloaddir
                 cp <- set cp "DEFAULT" "namingpatt" 
                       "%(album)s/%(safefilename)s"
                 return cp

startingcp = emptyCP {accessfunc = interpolatingAccess 10}

getCPName =
    do appdir <- getAppDir
       return $ appdir ++ "/hspod.conf"

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
