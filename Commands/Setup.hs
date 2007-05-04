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

module Commands.Setup(cmd, cmd_worker) where
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
import qualified Commands.Update
import qualified Commands.Download
import qualified Commands.Catchup
import qualified Commands.Add
import Data.ConfigFile
import System.IO
import Data.Either.Utils
import Data.String

i = infoM "setup"
w = warningM "setup"

--- This command is not available to the user by default -- it is
--- run on first execution of fetch

cmd = simpleCmd "setup" 
      "Perform initial configuration of hpodder -- NOT USER-VISIBLE" ""
      [] cmd_worker

cmd_worker gi ([], []) = lock $
    do cp <- loadCP
       putStr "Hello!  Welcome to hpodder!\n\n\
 \It looks like this is your first time running hpodder, so we're going\n\
 \to take care of a few very quick matters.\n\n\
 \First, where would you like to store your downloaded podcast episodes?\n\
 \You can just press Enter to accept the default location in the brackets,\n\
 \or enter your own location (full path, please!)\n\n\
 \  Download Location ["
       let defaultloc = forceEither $ get cp "DEFAULT" "downloaddir"
       putStr defaultloc
       putStr "]: "
       hFlush stdout
       dlloc_inp <- getLine
       putStr "\n\nOK!  Last question:  Would you like hpodder to\n\
\automatically subscribe you to a few sample podcasts?  This could be a nice\n\
\way to see what's out there.  You can always remove these or add your own\n\
\later.\n\n\
\  Subscribe to sample podcasts? [Y/n] "
       hFlush stdout
       subscribe_inp <- getLine
       cpname <- getCPName
       writeFile cpname $
                 (if (strip dlloc_inp) /= ""
                     then "[DEFAULT]\n\ndownloaddir = " ++ dlloc_inp ++ "\n"
                     else "\n") ++
                 "[general]\n\n; The following line tells hpodder that\n\
                 \; you have already gone through the intro.\n\
                 \showintro = no\n"
       case subscribe_inp of
         [] -> subscribeSamples gi
         'y':_ -> subscribeSamples gi
         'Y':_ -> subscribeSamples gi
         _ -> putStr "OK, as you wish, I won't add the sample subscriptions.\n\
                      \You can find the list of samples later in the hpodder\n\
                      \manual.\n"
       putStr "\n\nOK, hpodder is ready to run!  Each time you want to\n\
 \download new episodes, just run hpodder.  If you let me subscribe you\n\
 \to episodes, type hpodder and hit Enter to start the podcasts downloading!\n\
 \\nDon't forget to check the hpodder manual for more tips on hpodder!\n\n"

cmd_worker _ _ =
    fail $ "Invalid arguments to setup"

subscribeSamples gi =
    do putStr "OK, just a moment while I initialize those feeds for you...\n"
       mapM_ (\x -> Commands.Add.cmd_worker gi ([], [x])) sampleurls
       Commands.Update.cmd_worker gi ([], [])
       Commands.Catchup.cmd_worker gi ([("n", "1")], [])
       --Commands.Download.cmd_worker gi ([], [])

sampleurls = 
    ["http://soundofhistory.complete.org/files_soh/podcast.xml",
     "http://www.thelinuxlink.net/tllts/tllts.rss",
     "http://www.itconversations.com/rss/recentWithEnclosures.php",
     "http://www.sciam.com/podcast/sciam_podcast_r.xml",
     "http://www.npr.org/rss/podcast.php?id=510019",
     "http://amateurtraveler.com/podcast/rss.xml",
     "http://broadband.wgbh.org/amex/rss/podcast_np.xml",
     "http://www.npr.org/rss/podcast.php?id=700000"]

