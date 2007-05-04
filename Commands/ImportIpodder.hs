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

module Commands.ImportIpodder(cmd, cmd_worker) where
import Utils
import System.Log.Logger
import DB
import Download
import FeedParser
import Types
import Text.Printf
import Config
import Database.HDBC
import Control.Monad
import Utils
import System.Console.GetOpt
import System.Console.GetOpt.Utils
import qualified Commands.Update
import System.FilePath
import Data.List
import Data.String
import System.Directory
import Control.Exception

d = debugM "import-ipodder"
i = infoM "import-ipodder"
w = warningM "import-ipodder"

cmd = simpleCmd "import-ipodder" 
      "Import feeds and history from ipodder or castpodder" 
      ""
      [Option "" ["from"] (ReqArg (stdRequired "from") "PATH")
       "Location of ipodder data directory (default ~/.ipodder)"] 
      cmd_worker

cmd_worker gi (args, []) = lock $
    do ipodderpath <- case lookup "from" args of
                        Nothing -> do getAppUserDataDirectory "ipodder"
                        Just x -> return x
       i "Scanning ipodder podcast list and adding new podcasts to hpodder..."
       pc <- newpodcasts ipodderpath gi
       i $ printf "Added %d podcasts" (length pc)

       i "Loading new feeds..."
       Commands.Update.cmd_worker gi ([], map (show . castid) pc)
       commit (gdbh gi)
       
       i "Now importing iPodder history..."
       history <- loadhistory ipodderpath
       prochistory gi pc history
       commit (gdbh gi)
       i "Done."

cmd_worker _ _ = 
    fail "Unknown arg to import-ipodder; see hpodder import-ipodder --help"

prochistory _ [] _ = return ()
prochistory gi (pc:xs) history =
    do episodes <- getEpisodes (gdbh gi) pc
       -- Force episodes to be consumed before proceeding
       evaluate (length episodes)
       d $ printf "Considering %d episode(s) for podcast %d" (length episodes)
          (castid pc)
       mapM_ procep episodes
       prochistory gi xs history
    where procep ep = 
              if (snd . splitFileName . epurl $ ep) `elem` history
                 && (epstatus ep) `elem` [Pending, Error]
                 then do d $ printf "Adjusting episode %d" (epid ep)
                         updateEpisode (gdbh gi) (ep {epstatus = Skipped})
                         return ()
                 else return ()

newpodcasts id gi =
    do favorites <- readFile (id ++ "/favorites.txt")
       let newurls = filter (not . isPrefixOf "#") . map strip . lines 
                     $ favorites
       existinglist <- getPodcasts (gdbh gi)
       let existingurls = map feedurl existinglist
       let urlstoadd = filter (\newurl -> not (newurl `elem` existingurls))
                       newurls
       let podcaststoadd = map (\url -> Podcast {castid = 0, 
                                                 castname = "",
                                                 feedurl = url,
                                                 pcstatus = Enabled,
                                                 lastupdate = Nothing}) urlstoadd
                           
       newpcs <- mapM (addPodcast (gdbh gi)) podcaststoadd
       commit (gdbh gi)
       return newpcs

loadhistory :: String -> IO [String]
loadhistory id =
    do historyfile <- readFile (id ++ "/history.txt")
       return $ filter (/= "") . map strip . lines $ historyfile

