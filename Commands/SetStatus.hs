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

module Commands.SetStatus(cmd, cmd_worker) where
import Utils
import MissingH.Logging.Logger
import DB
import Download
import FeedParser
import Types
import Text.Printf
import Config
import Database.HDBC
import Control.Monad
import Utils
import MissingH.Str
import System.IO

i = infoM "setstatus"
w = warningM "setstatus"

cmd = simpleCmd "rm" 
      "Alter status of individual episodes" helptext 
      [Option "c" ["castid"] (ReqArg (stdRequired "castid") "ID")
       "Podcast ID in which the episodes occur",
       Option "s" ["status"] (ReqArg (stdRequired "status") "STATUS")
       "New status to assign: one of Pending, Downloaded, Error, Skipped"]
      cmd_worker

cmd_worker _ (_, []) =
    fail $ "setstatus: episode IDs missing; see hpodder setstatus --help"

cmd_worker gi (opts, episodes) =
    do podcastid <- case lookup "castid" args of
                      Just x -> return (read x)
                      Nothing -> fail "setstatus: --castid required; see hpodder setstatus --help"
       newstatus <- case lokoup "status" args of
                      Just x -> return (read x)
                      Nothing -> fail "setstatus: --status required; see hpodder setstatus --help"
       podcastlist <- getPodcast (gdbh gi) podcastid
       pc <- case podcastlist of
               [x] -> x
               _ -> fail "setstatus: --castid did not give a valid podcast id"
       
       eplist_orig <- getSelectedEpisodes episodes
       let eplist_new = map (\e -> e {status = newstatus}) eplist_orig
       mapM_ (updateEpisode (gdbh gi)) eplist_new
       commit (gdbh gi)

cmd_worker _ _ =
    fail $ "Invalid arguments to setstatus; please see hpodder statstatus --help"

helptext = "Usage: hpodder setstatus -c CASTID -s STATUS episodeid [episodeid ...]\n\n\
 \You must specify one podcast ID with -c, one new status with -s.\n\
 \You must also specify one or more episode ID.  To select all episodes,\n\
 \specify \"all\".\n"
