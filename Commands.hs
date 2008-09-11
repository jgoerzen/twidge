{-
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
   Module     : Commands
   Copyright  : Copyright (C) 2006-2008 John Goerzen
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

import qualified Commands.Follow
import qualified Commands.Ls
import qualified Commands.Setup
import qualified Commands.Update

--allCommands :: [(String, Command)]
allCommands = 
    [Commands.Follow.follow,
     Commands.Ls.lsfollowers,
     Commands.Ls.lsfollowing,
     Commands.Ls.lsrecent,
     Commands.Ls.lsreplies,
     lscommands,
     Commands.Setup.setup,
     Commands.Follow.unfollow,
     Commands.Update.update
    ]

lscommands = 
    simpleCmd "lscommands" "Display a list of all available commands" ""
                  [] lscommands_worker

lscommands_worker _ _ _ =
    do putStrLn "All available commands:"
       printf "%-20s %s\n" "Name" "Description"
       putStrLn "-------------------- -------------------------------------------------------"
       mapM_ (\(_, x) -> printf "%-20s %s\n" (cmdname x) (cmddescrip x))
             allCommands
                 
