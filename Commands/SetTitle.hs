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

module Commands.SetTitle(cmd, cmd_worker) where
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
import Data.String
import System.IO
import System.Console.GetOpt
import System.Console.GetOpt.Utils
import Control.Exception

i = infoM "settitle"
w = warningM "settitle"
d = debugM "settitle"

cmd = simpleCmd "settitle" 
      "Alter title of a podcast" helptext 
      [Option "c" ["castid"] (ReqArg (stdRequired "castid") "ID")
       "Podcast ID to modify",
       Option "t" ["title"] (ReqArg (stdRequired "title") "TITLE")
       "New title for this podcast"]
      cmd_worker

cmd_worker gi (args, []) =  lock $
    do podcastid <- case lookup "castid" args of
                      Just x -> return (read x)
                      Nothing -> fail "settitle: --castid required; see hpodder settitle --help"
       newtitle <- case lookup "title" args of
                      Just x ->  return x
                      Nothing -> fail "settitle: --title required; see hpodder settitle --help"
       pc <- getPodcast (gdbh gi) podcastid
       case pc of
          [x] -> updatePodcast (gdbh gi) (x {castname = newtitle})
          _ -> fail $ "Invalid podcast ID given"
       commit (gdbh gi)

cmd_worker gi (_, _) =
    fail $ "settitle: unrecognized arguments; see hpodder settitle --help"


helptext = "Usage: hpodder settitle -c CASTID -t 'TITLE'\n\n\
 \You must specify one podcast ID with -c and the new title with -t.\n\
 \If the title contains spaces, you must enclose it in quotes.\n"
