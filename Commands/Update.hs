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

module Commands.Update(cmd, cmd_worker) where
import Utils
import System.Log.Logger
import DB
import Download
import DownloadQueue
import Data.Progress.Meter
import Data.Progress.Tracker
import FeedParser
import Types
import Text.Printf
import Config
import Database.HDBC
import Control.Monad
import Utils
import Data.String
import System.Exit
import System.Posix.Process
import System.Directory
import System.IO
import Data.List
import Data.Either.Utils
import Data.ConfigFile(get)

i = infoM "update"
w = warningM "update"

cmd = simpleCmd "update" 
      "Re-scan all feeds and update list of needed downloads" helptext 
      [] cmd_worker

cmd_worker gi ([], casts) = lock $
    do podcastlist' <- getSelectedPodcasts (gdbh gi) casts
       let podcastlist = filter_disabled podcastlist'
       i $ printf "%d podcast(s) to consider\n" (length podcastlist)
       updatePodcasts gi podcastlist
       return ()

cmd_worker _ _ =
    fail $ "Invalid arguments to update; please see hpodder update --help"

updatePodcasts gi podcastlist =
    do ft <- getFeedTmp
       emptyDir ft
       easyDownloads "update" getFeedTmp False
                     (\pt -> mapM (podcast2dlentry pt) podcastlist)
                     procStart
                     (updateThePodcast gi)
       emptyDir ft
    where podcast2dlentry pt podcast = 
              do cpt <- newProgress (show . castid $ podcast) 1
                 addParent cpt pt
                 return $ DownloadEntry {dlurl = feedurl podcast,
                                         usertok = podcast,
                                         dlname = (show . castid $ podcast),
                                         dlprogress = cpt}

                 --removeFile ((\(_, _, fp, _) -> fp) dltok)
          procStart pt meter dlentry dltok =
              writeMeterString stdout meter $
                 "Get: " ++ show (castid . usertok $ dlentry) ++ " "
                 ++ (take 65 . castname . usertok $ dlentry) ++ "\n"
                  

updateThePodcast gi pt meter dlentry dltok status result =
    do incrP (dlprogress dlentry) 1
       let pc = usertok dlentry
       feed <- getFeed meter pc (result, status) dltok
       case feed of
         Nothing ->                         -- some problem with the feed
           case status of 
             Terminated sigINT -> return () -- Ctrl-C is not a tackable error
             _ -> do curtime <- now
                     let newpc = considerDisable gi
                           (pc {lastattempt = Just curtime,
                                failedattempts = 1 + failedattempts pc})
                     updatePodcast (gdbh gi) newpc
                     commit (gdbh gi)
                     when (pcenabled newpc == PCErrorDisabled) $
                        i ("   Podcast " ++ castname newpc ++ " disabled due to errors.")
         Just f -> do newpc <- updateFeed gi pc f
                      curtime <- now
                      updatePodcast (gdbh gi) 
                                    (newpc {lastupdate = Just curtime,
                                            lastattempt = Just curtime,
                                            failedattempts = 0})
                      --i $ "   Podcast Title: " ++ (castname newpc)
                      commit (gdbh gi)

considerDisable gi pc = forceEither $
    do faildays <- get (gcp gi) (show (castid pc)) "podcastfaildays"
       failattempts <- get (gcp gi) (show (castid pc)) "podcastfailattempts"
       let lupdate = case lastupdate pc of
                            Nothing -> 0
                            Just x -> x
       let timepermitsdel = case lastattempt pc of
                                Nothing -> True
                                Just x -> x - lupdate > faildays * 60 * 60 * 24
       case pcenabled pc of
         PCUserDisabled -> return pc
         PCErrorDisabled -> return pc
         PCEnabled -> return $
           pc {pcenabled =
               if (failedattempts pc > failattempts) && timepermitsdel
                  then PCErrorDisabled
                  else PCEnabled
                  }

updateFeed gi pcorig f =
    do count <- foldM (updateEnc gi pc) 0 (items f)
       --i $ printf "   %d new episodes" count
       return pc
    where pc = pcorig {castname = newname}
          newname = if (castname pcorig) == ""
                       then strip . sanitize_basic $ channeltitle f
                       else (castname pcorig)

updateEnc gi pc count item = 
    do newc <- addEpisode (gdbh gi) (item2ep pc item)
       return $ count + newc

getFeed meter pc (result, status) dltok =
       case (result, status) of
         (Success, _) -> 
             do feed <- parse (tokpath dltok) (feedurl pc)
                case feed of
                  Right f -> return $ Just (f {items = reverse (items f)})
                  Left x -> do writeMeterString stderr meter $
                                 " *** " ++ (show . castid $ pc) ++ 
                                 ": Failure parsing feed: " ++ x ++ "\n"
                               return Nothing
         (Failure, Terminated sigINT) -> do w "\n   Ctrl-C hit; aborting!"
                                            exitFailure
         _ -> do writeMeterString stderr meter $
                  " *** " ++ (show . castid $ pc) ++ ": Failure downloading feed\n"
                 return Nothing

helptext = "Usage: hpodder update [castid [castid...]]\n\n" ++ genericIdHelp ++
 "\nRunning update will cause hpodder to look at each requested podcast.  It\n\
 \will download the feed for each one and update its database of available\n\
 \episodes.  It will not actually download any episodes; see the download\n\
 \command for that."
