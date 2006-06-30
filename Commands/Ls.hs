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

module Commands.Ls(lscasts, lscasts_worker, lsepisodes, lseps) where
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
       printf "%-4s %-7s %s\n" "ID" "Pnd/Tot" "Title"
       when (islong) (printf "     URL\n")
       mapM_ printpc (sort pc)
    where islong = lookup "l" opts == Just ""
          printpc pc = do pend <- quickQuery (gdbh gi) "SELECT COUNT(*) FROM \
                                  \episodes WHERE castid = ? AND \
                                  \status = 'Pending'" [toSql (castid pc)]
                          tot <- quickQuery (gdbh gi) "SELECT COUNT(*) FROM \
                                  \episodes WHERE castid = ?" 
                                  [toSql (castid pc)]
                          let (npend, ntot) = case (pend, tot) of
                                                ([[x]], [[y]]) -> 
                                                    (fromSql x, fromSql y)
                                                _ -> error "Bad count result"
                          printf "%-4d %3d/%3d %s\n" (castid pc) 
                                     (npend::Int) (ntot::Int) (castname pc)
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

lseps = simpleCmd "lseps" "Alias for lsepisodes" lsepisodes_help
             [Option "l" [] (NoArg ("l", "")) "Long format display -- include URLs in putput"] lsepisodes_worker

lsepisodes_worker gi (opts, casts) =
    do pc <- getSelectedPodcasts (gdbh gi) casts
       printf "%-5s %-5s %-4s %-60.60s\n" "CstId" "EpId" "Stts" "Episode Title"
       when (islong) (printf "            Episode URL\n")
       eps <- mapM (getEpisodes (gdbh gi)) pc
       mapM_ printep (concat eps)
    where printep ep =
              do printf "%-5d %-5d %-4s %-60.60s\n" 
                            (castid (podcast ep)) (epid ep) 
                            (take 4 . show $ epstatus ep) (eptitle ep)
                 when (islong) (printf "            %s\n" (epurl ep))
          islong = lookup "l" opts == Just ""

lsepisodes_help =
 "Usage: hpodder lsepisodes [-l] [castid [castid...]]\n\n" ++ genericIdHelp ++
 "\nIf no podcast ID is given, then \"all\" will be used.  You can find your\n\
 \podcast IDs with \"hpodder lscasts\".\n"