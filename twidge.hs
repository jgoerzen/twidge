{- component
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

{- |
   Module     : Main
   Copyright  : Copyright (C) 2006-2008 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

import Config
import System.Log.Logger
import System.Log.Handler.Simple
import System.Console.GetOpt.Utils
import System.Console.GetOpt
import System.Environment
import Data.List
import System.Exit
import Commands
import Types
import Control.Monad
import Data.ConfigFile(emptyCP,get)
import System.IO
import Paths_twidge(version)
import Data.Version

main = 
    do updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       
       case getOpt RequireOrder options argv of
         (o, n, []) -> worker o n
         (_, _, errors) -> usageerror (concat errors) -- ++ usageInfo header options)
       
options = [Option "d" ["debug"] (NoArg ("d", "")) "Enable debugging",
           Option "c" ["config"] (ReqArg (stdRequired "c") "FILE")
                  "Use specified config file",
           Option "" ["help"] (NoArg ("help", "")) "Display this help"]

expandAlias cp cmd = either (const cmd) id $ get cp "alias" cmd

worker args commandargs =
    do when (lookup "help" args == Just "") $ usageerror ""
       when (lookup "d" args == Just "") 
            (updateGlobalLogger "" (setLevel DEBUG))
       handler <- streamHandler stderr DEBUG -- stdout has issues with HSH
       updateGlobalLogger "" (setHandlers [handler])
       let commandname = head cmdargs
       cp <- if commandname `elem` ["lscommands", "setup"] -- no config file needed
                then loadCP True (lookup "c" args)
                else loadCP False (lookup "c" args)
       let cmdargs' = (words $ expandAlias cp commandname) ++ (tail cmdargs)
       let commandname' = head cmdargs'
       case lookup commandname' allCommands of
         Just command -> execcmd command (tail cmdargs') (lookup "c" args) cp
         Nothing -> if commandname == commandname'
                      then usageerror ("Invalid command name " ++ commandname)
                      else usageerror ("Invalid command name " ++ commandname
                                      ++ " (aliased to " ++ commandname' ++ ")")
       where cmdargs = case commandargs of
                         [] -> ["help"]
                         x -> x

usageerror errormsg =
    do putStrLn errormsg
       putStrLn (usageInfo header options)
       putStrLn "Run \"twidge lscommands\" for a list of available commands.\n\
                \Run \"twidge command --help\" for help on a particular command.\n"
       putStr $ "This is Twidge, version " ++ showVersion version
       putStrLn $ ".  Copyright (c) 2008-2010 John Goerzen"
       exitFailure

header = "Usage: twidge [global-options] command [command-options]\n\n\
         \Available global-options are:\n"
