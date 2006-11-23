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

module Commands.Update(cmd, cmd_worker) where
import Utils
import MissingH.Logging.Logger
import DB
import Download
import DownloadQueue
import MissingH.ProgressMeter
import MissingH.ProgressTracker
import FeedParser
import Types
import Text.Printf
import Config
import Database.HDBC
import Control.Monad
import Utils
import MissingH.Str
import System.Exit
import System.Posix.Process

i = infoM "update"
w = warningM "update"

cmd = simpleCmd "update" 
      "Re-scan all feeds and update list of needed downloads" helptext 
      [] cmd_worker

cmd_worker gi ([], casts) =
    do podcastlist' <- getSelectedPodcasts (gdbh gi) casts
       let podcastlist = filter_disabled podcastlist'
       i $ printf "%d podcast(s) to consider\n" (length podcastlist)
       updatePodcasts gi podcastlist
       mapM_ (updateThePodcast gi) podcastlist

cmd_worker _ _ =
    fail $ "Invalid arguments to update; please see hpodder update --help"

updatePodcasts gi podcastlist =
    do maxthreads <- getMaxThreads (gcp gi)
       progressinterval <- getProgressInterval (gcp gi)
       basedir <- getFeedTmp
       pt <- newProgress "update" (length podcastlist)
       meter <- simpleNewMeter pt
       autoDisplayMeter meter progressinterval displayMeter
       results <- runDownloads (callback pt meter) basedir False 
                  dlentries maxthreads
       
    where dlentries = map podcast2dlentry podcastlist
          podcast2dlentry podcast = DownloadEntry {dlurl = feedurl podcast,
                                                   usertok = podcast}
          callback pt meter dlentry (DLStarted _) =
                 writeMeterString meter $
                      "Get:" ++ show (castid . usertok $ dlentry) ++ " " ++
                      (take 65 . castname . usertok $ dlentry) ++ "\n"
          callback pt meter dlentry (DLEneded (dltok, result)) =
              do incrP pt 1
                 feed <- getFeed (usertok dlentry) result dltok
                 updateThePodcast gi (usertok dlentry) feed
                 
                 

updateThePodcast gi pc feed =
       case feed of
         Nothing -> return ()
         Just f -> do newpc <- updateFeed gi pc f
                      curtime <- now
                      updatePodcast (gdbh gi) 
                                    (newpc {lastupdate = Just curtime})
                      --i $ "   Podcast Title: " ++ (castname newpc)
       commit (gdbh gi)

updateFeed gi pcorig f =
    do count <- foldM (updateEnc gi pc) 0 (items f)
       i $ printf "   %d new episodes" count
       return pc
    where pc = pcorig {castname = newname}
          newname = if (castname pcorig) == ""
                       then strip . sanitize_basic $ channeltitle f
                       else (castname pcorig)

updateEnc gi pc count item = 
    do newc <- addEpisode (gdbh gi) (item2ep pc item)
       return $ count + newc

getFeed pc result (_, _, dlfilename) =
       case result of
         (Success, _) -> 
             do feed <- parse dlfilename (feedurl pc)
                case feed of
                  Right f -> return $ Just (f {items = reverse (items f)})
                  Left x -> do w $ "   Failure parsing feed: " ++ x
                               return Nothing
         (TempFail, Terminated sigINT) -> do w "   Ctrl-C hit; aborting!"
                                             exitFailure
         _ -> do w "   Failure downloading feed; will attempt again on next update"
                 return Nothing

helptext = "Usage: hpodder update [castid [castid...]]\n\n" ++ genericIdHelp ++
 "\nRunning update will cause hpodder to look at each requested podcast.  It\n\
 \will download the feed for each one and update its database of available\n\
 \episodes.  It will not actually download any episodes; see the download\n\
 \command for that."