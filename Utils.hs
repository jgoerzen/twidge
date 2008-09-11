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

{- |
   Module     : Utils
   Copyright  : Copyright (C) 2006-2007 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

module Utils where
import System.Console.GetOpt.Utils
import System.Console.GetOpt
import Types
import System.Exit
import Config
import System.Directory
import Data.List.Utils
import System.Time
import System.Time.Utils
import System.IO
import System.Posix.IO
import Control.Exception(finally)
import Data.ConfigFile

simpleCmd :: String             -- ^ Command name
          -> String             -- ^ Command description
          -> String             -- ^ Command help text
          -> [OptDescr (String, String)] -- ^ Option descriptions
          -> (Maybe String -> ConfigParser -> ([(String, String)], [String]) -> IO ()) -- ^ Function to call 
          -> (String, Command)
simpleCmd name descrip helptext optionsinp func =
    (name, Command {cmdname = name, cmddescrip = descrip,
                    execcmd = worker})
    where options =
              optionsinp ++ [Option "" ["help"] (NoArg ("help", "")) "Display this help"]
          worker argv cpath gi =
              case getOpt RequireOrder options argv of
                (o, n, []) -> 
                    if (lookup "help" o == Just "") 
                       then usageerror []
                       else func cpath gi (o, n)
                (_, _, errors) -> usageerror (concat errors)
          usageerror errormsg =
              do putStrLn $ "Error processing arguments for command " ++ 
                          name ++ ":"
                 putStrLn errormsg
                 putStrLn (usageInfo header options)
                 putStrLn helptext
                 exitFailure
          header = "Available command-options for " ++ name ++ " are:\n"
                                                               
{-
lock func =
    do appdir <- getAppDir
       lockh <- openFile (appdir ++ "/.lock") WriteMode
       lockfd <- handleToFd lockh
       catch (placelock lockfd) errorhandler
       r <- finally func (releaselock lockfd)
       return r

    where placelock lockfd = setLock lockfd (WriteLock, AbsoluteSeek, 0, 0)
          releaselock lockfd = do
               setLock lockfd (Unlock, AbsoluteSeek, 0, 0)
               closeFd lockfd
          errorhandler _ =
              do putStrLn "Aborting because another hpodder is already running"
                 exitFailure
-}

sanitize_basic inp =
    case filter (\c -> not (c `elem` "\n\r\t\0")) inp of
      ('-':x) -> ('_':x)          -- Strip leading hyphen
      x -> x

sanitize_fn inp =
    case map worker . sanitize_basic $ inp of
      [] -> "UNKNOWN"
      x -> x
    where worker x = if x `elem` ";/|!`~ *?%^&(){}[]\\'\"<>:" 
                         then '_'
                         else x

now :: IO Integer
now = do ct <- getClockTime
         return (clockTimeToEpoch ct)

