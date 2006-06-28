{- hspod component
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

{- |
   Module     : Utils
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

module Utils where
import MissingH.GetOpt
import System.Console.GetOpt
import Types
import System.Exit

simpleCmd :: String -> String -> [OptDescr a] -> (([a], [String]) -> IO ()) -> 
             (String, Command)
simpleCmd name descrip options func =
    (name, Command {cmdname = name, cmddescrip = descrip,
                    execcmd = worker})
    where worker argv =
              case getOpt RequireOrder options argv of
                (o, n, []) -> func (o, n)
                (_, _, errors) -> usageerror (concat errors)
          usageerror errormsg =
              do putStrLn $ "Error processing arguments for command " ++ 
                          name ++ ":"
                 putStrLn errormsg
                 putStrLn (usageInfo header options)
                 exitFailure
          header = "Usage: hspod [global-options] " ++ name ++ " [command-options]\n\n\
                    \Run hspod --help for help on global options.\n\
                    \\n\
                    \Available command-options for " ++ name ++ " are:\n"
                                                               

