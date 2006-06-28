{- hpodder component
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

module Commands.Ls(lscasts, lsepisodes) where
import Utils
import MissingH.Logging.Logger
import DB
import Types
import Database.HDBC
import Text.Printf
import Control.Monad
import System.Console.GetOpt
import Data.List

i = infoM "ls"

--------------------------------------------------
-- lscasts
--------------------------------------------------

lscasts = simpleCmd "lscasts" "List all configured podcasts on the system"
             lscasts_help
          [Option "l" [] (NoArg ("l", "")) "Long format display -- include URLs in output"] lscasts_worker

lscasts_worker gi (opts, casts) =
    do pc <- getSelectedPodcasts (gdbh gi) casts
       printf "%-4s %s\n" "ID" "Title"
       when (islong) (printf "     URL\n")
       mapM_ printpc (sort pc)
    where islong = lookup "l" opts == Just ""
          printpc pc = do printf "%-4d %s\n" (castid pc) (castname pc)
                          when (islong) (printf "     %s\n" (feedurl pc))

lscasts_help =
 "Usage: hpodder lscasts [-l] [castid [castid...]]\n\n" ++ genericIdHelp ++
 "\nIf no ID is given, then \"all\" will be used.\n"

--------------------------------------------------
-- lsepisodes
--------------------------------------------------

lsepisodes = simpleCmd "lsepisodes" "List episodes in hspodder database"
               lsepisodes_help
             [Option "l" [] (NoArg ("l", "")) "Long format display -- include URLs in putput"] lsepisodes_worker

lsepisodes_worker gi (opts, casts) =
    do pc <- getSelectedPodcasts (gdbh gi) casts
       printf "%-5s %-5s %-65.65s\n" "CstId" "EpId" "Episode Title"
       when (islong) (printf "            Episode URL\n")
       eps <- mapM (getEpisodes (gdbh gi)) pc
       mapM_ printep (concat eps)
    where printep ep =
              do printf "%-5d %-5d %-65.65s\n" (castid (podcast ep)) (epid ep)
                        (eptitle ep)
                 when (islong) (printf "            %s\n" (epurl ep))
          islong = lookup "l" opts == Just ""

lsepisodes_help =
 "Usage: hpodder lsepisodes [-l] [castid [castid...]]\n\n" ++ genericIdHelp ++
 "\nIf no podcast ID is given, then \"all\" will be used.  You can find your\n\
 \podcast IDs with \"hpodder lscasts\".\n"