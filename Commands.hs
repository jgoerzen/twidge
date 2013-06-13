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
   Copyright  : Copyright (C) 2006-2010 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}
module Commands where

import Text.Printf
import Utils
import Types

import qualified Commands.FollowBlock
import qualified Commands.Ls
import qualified Commands.Setup
import qualified Commands.Update

--allCommands :: [(String, Command)]
allCommands = 
    [Commands.Update.dmsend,
     Commands.FollowBlock.block,
     Commands.FollowBlock.follow,
     Commands.Ls.lsarchive,
     lscommands,
     Commands.Ls.lsdm,
     Commands.Ls.lsdmarchive,
     Commands.Ls.lsblocking,
     Commands.Ls.lsfollowers,
     Commands.Ls.lsfollowing,
     Commands.Ls.lsrecent,
     Commands.Ls.lsreplies,
     Commands.Ls.lsrtreplies,
     Commands.Ls.status,
     Commands.Setup.setup,
     Commands.FollowBlock.unblock,
     Commands.FollowBlock.unfollow,
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
                 
