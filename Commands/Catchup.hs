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

module Commands.Catchup(cmd, cmd_worker) where
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

i = infoM "catchup"
w = warningM "catchup"

cmd = simpleCmd "catchup" 
      "Tell hpodder to ignore old undownloaded episodes" helptext
      [Option "n" ["--number-eps"] [] (ReqArg (stdRequired "n") "NUM")
       "Number of episodes to allow to download (default 1)"] 
      cmd_worker

cmd_worker gi (args, casts) =
    do podcastlist <- getSelectedPodcasts (gdbh gi) casts
       let n = case lookup "n" args of
                 Nothing -> 1
                 Just y -> read y
       i $ printf "%d podcast(s) to consider\n" (length podcastlist)
       mapM_ (catchupThePodcast gi n) podcastlist

cmd_worker _ _ =
    fail $ "Invalid arguments to catchup; please see hpodder catchup --help"

catchupThePodcast gi n pc =
    do i $ printf " * Podcast %d: %s" (castid pc) (feedurl pc)
       eps <- getEpisodes (gdbh gi) pc
       let epstoproc = take (length eps - n) eps
       mapM procEp epstoproc
    where procEp ep = 
              updateEpisode dbh (ep {epstatus = newstatus})
              where newstatus = 
                        case epstatus ep of
                          Pending -> Skipped
                          Error -> Skipped
                          x -> x
                                   

updateFeed gi pcorig f =
    do count <- foldM (updateEnc gi pc) 0 (items f)
       i $ printf "   %d new episodes" count
       return pc
    where pc = pcorig {castname = sanitize_basic (channeltitle f)}

updateEnc gi pc count item = 
    do newc <- addEpisode (gdbh gi) (item2ep pc item)
       return $ count + newc

getFeed pc =
    do result <- getURL (feedurl pc) "feed.xml"
       case result of
         Success -> 
             do feed <- parse "feed.xml" (feedurl pc)
                return $ Just (feed {items = reverse (items feed)})
         _ -> do w "   Failure downloading feed"
                 return Nothing

helptext = "Usage: hpodder update [castid [castid...]]\n\n" ++ genericIdHelp ++
 "\nRunning update will cause hpodder to look at each requested podcast.  It\n\
 \will download the feed for each one and update its database of available\n\
 \episodes.  It will not actually download any episodes; see the download\n\
 \command for that."