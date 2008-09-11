{-
Copyright (C) 2006-2008 John Goerzen <jgoerzen@complete.org>

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

module Commands.Setup(setup) where
import Utils
import System.Log.Logger
import Types
import Text.Printf
import System.Console.GetOpt
import Data.List
import Download
import FeedParser
import Data.ConfigFile
import System.IO
import Data.Either.Utils
import Data.Char
import Config
import Control.Monad(when)

i = infoM "setup"

--------------------------------------------------
-- lscasts
--------------------------------------------------

setup = simpleCmd "setup" "Interactively configure twidge for first-time use"
             setup_help
             [] setup_worker

setup_worker cpath cp _ =
    do when (has_option cp "DEFAULT" "username" || 
             has_option cp "DEFAULT" "password")
            (confirmSetup)
       hSetBuffering stdout NoBuffering
       putStrLn "\nWelcome to twidge.  We will now configure twidge for your"
       putStrLn "use.  This will be quick and easy!\n"
       putStrLn "First, what is your usename?\n"
       putStr   "Username: "
       username <- getLine
       putStrLn $ "\nWelcome, " ++ username ++ "!  Now I'll need your password.\n"
       putStr   "Password: "
       password <- getLine
       let newcp = forceEither $ set cp "DEFAULT" "username" (esc username)
       let newcp' = forceEither $ set newcp "DEFAULT" "password" (esc password)
       
       writeCP cpath newcp'
       
       putStrLn "\n\ntwidge has now been configured for you.\n"
    where confirmSetup =
              do putStrLn "\nIt looks like you have already configured twidge."
                 putStrLn "If we continue, I may remove your existing"
                 putStrLn "configuration.  Would you like to proceed?"
                 putStr   "\nYES or NO: "
                 c <- getLine
                 if (map toLower c) == "yes"
                    then return ()
                    else permFail "Aborting configuration at user request."
          esc x = concatMap fix x
          fix '%' = "%%"
          fix x = [x]

setup_help =
 "Usage: twidge setup\n\n"
