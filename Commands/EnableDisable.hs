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

module Commands.EnableDisable(cmd_enable, cmd_disable, cmd_worker) where
import Utils
import System.Log.Logger
import DB
import Download
import FeedParser
import Types
import Text.Printf
import Config
import Database.HDBC
import Control.Exception(evaluate)
import Control.Monad
import Utils
import Data.String
import System.IO

i = infoM "enable/disable"
w = warningM "enable/disable"
d = debugM "enable/disable"

cmd_enable = simpleCmd "enable" 
             "Enable a podcast that was previously disabled" helptext 
             [] (cmd_worker "enable" PCEnabled)

cmd_disable = simpleCmd "disable"
              "Stop updating and downloading given podcasts" helptext_disable
              [] (cmd_worker "disable" PCUserDisabled)

cmd_worker cmd _ gi ([], []) =
    fail $ cmd ++ " requires a podcast ID to remove; please see hpodder "
           ++ cmd ++ " --help"

cmd_worker cmd newstat gi ([], casts) = lock $
    do podcastlist <- getSelectedPodcasts (gdbh gi) casts
       evaluate (length podcastlist)
       d $ "Setting " ++ (show . length $ podcastlist) ++ " podcasts to " ++
         show (newstat)
       mapM_ (\x -> updatePodcast (gdbh gi) 
                    (x {pcenabled = newstat,
                        failedattempts = if newstat == PCEnabled then 0 
                                            else failedattempts x})) 
             podcastlist
       commit (gdbh gi)
           

cmd_worker cmd _ _ _ =
    fail $ "Invalid arguments to enable; please see hpodder " ++ cmd ++ " --help"

helptext = "Usage: hpodder enable castid [castid...]\n\n" ++ 
 "\nEnables selected podcasts for downloading and updating\n"

helptext_disable = "Usage: hpodder disable castid [castid...]\n\n" ++
 "\nDisables selected podcasts -- they will no longer be downloaded or\n\
 \updated until re-enabled.\n"
