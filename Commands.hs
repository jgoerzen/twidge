{- hpodder component
Copyright (C) 2006-2007 John Goerzen <jgoerzen@complete.org>

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
   Module     : Commands
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}
module Commands where

import Data.List
import Text.Printf
import Utils
import Types
import Config
import Data.ConfigFile
import Data.Either.Utils

import qualified Commands.Add
import qualified Commands.Ls
import qualified Commands.Update
import qualified Commands.Download
import qualified Commands.Setup
import qualified Commands.Catchup
import qualified Commands.ImportIpodder
import qualified Commands.Rm
import qualified Commands.SetStatus
import qualified Commands.SetTitle
import qualified Commands.EnableDisable

--allCommands :: [(String, Command)]
allCommands = 
    [Commands.Add.cmd,
     Commands.Catchup.cmd,
     Commands.EnableDisable.cmd_disable,
     Commands.Download.cmd,
     Commands.EnableDisable.cmd_enable,
     fetch,
     Commands.ImportIpodder.cmd,
     Commands.Ls.lscasts,
     lscommands,
     Commands.Ls.lsepisodes,
     Commands.Ls.lseps,
     Commands.Rm.cmd,
     Commands.SetStatus.cmd,
     Commands.SetTitle.cmd,
     Commands.Update.cmd]

lscommands = 
    simpleCmd "lscommands" "Display a list of all available commands" ""
                  [] lscommands_worker

lscommands_worker _ _ =
    do putStrLn "All available commands:"
       printf "%-20s %s\n" "Name" "Description"
       putStrLn "-------------------- -------------------------------------------------------"
       mapM_ (\(_, x) -> printf "%-20s %s\n" (cmdname x) (cmddescrip x))
             allCommands
                 

fetch = 
    simpleCmd "fetch" "Scan feeds, then download new episodes" fetch_help
              [] fetch_worker

fetch_worker gi ([], casts) =
    do cp <- loadCP
       let showintro = forceEither $ get cp "general" "showintro"
       if showintro 
              then Commands.Setup.cmd_worker gi ([], [])
              else do Commands.Update.cmd_worker gi ([], casts)
                      Commands.Download.cmd_worker gi ([], casts)
    
fetch_worker _ _ =
    fail $ "Invalid arguments to fetch; please see hpodder fetch --help"

fetch_help = "Usage: hpodder fetch [castid [castid...]]\n\n" ++ 
             genericIdHelp  ++
 "\nThe fetch command will cause hpodder to scan all feeds (as with\n\
 \\"hpodder update\") and then download all new episodes (as with\n\
 \\"hpodder download\").\n"
