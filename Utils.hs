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
import Config
import System.Directory
import DB
import Database.HDBC
import MissingH.List

simpleCmd :: String -> String -> String -> [OptDescr (String, String)] 
          -> (GlobalInfo -> ([(String, String)], [String]) -> IO ()) 
          -> (String, Command)
simpleCmd name descrip helptext optionsinp func =
    (name, Command {cmdname = name, cmddescrip = descrip,
                    execcmd = worker})
    where options =
              optionsinp ++ [Option "" ["help"] (NoArg ("help", "")) "Display this help"]
          worker argv gi =
              case getOpt RequireOrder options argv of
                (o, n, []) -> 
                    if (lookup "help" o == Just "") 
                       then usageerror []
                       else func gi (o, n)
                (_, _, errors) -> usageerror (concat errors)
          usageerror errormsg =
              do putStrLn $ "Error processing arguments for command " ++ 
                          name ++ ":"
                 putStrLn errormsg
                 putStrLn (usageInfo header options)
                 putStrLn helptext
                 exitFailure
          header = "Available command-options for " ++ name ++ " are:\n"
                                                               

initDirs = 
    do appdir <- getAppDir
       mapM_ mkdir [appdir, appdir ++ "/feedxfer", appdir ++ "/enclosurexfer"]
       where mkdir = createDirectoryIfMissing True

sanitize_basic inp =
    case filter (\c -> not (c `elem` "\n\r\t\0")) inp of
      ('-':x) -> ('_':x)          -- Strip leading hyphen
      x -> x

sanitize_fn =
    map worker . sanitize_basic
    where worker x = if x `elem` ";/|!`~ *?%^&(){}[]\\'\"<>:" 
                         then '_'
                         else x

genericIdHelp =
 "You can optionally specify one or more podcast IDs.  If given,\n\
  \only those IDs will be selected for processing.\n\n\
  \The special id \"all\" will select all podcast IDs.\n"

getSelectedPodcasts dbh [] = getSelectedPodcasts dbh ["all"]
getSelectedPodcasts dbh ["all"] = getPodcasts dbh
getSelectedPodcasts dbh podcastlist =
    do r <- mapM (getPodcast dbh) (map read podcastlist)
       return $ uniq $ concat r

getSelectedEpisodes :: Connection -> Podcast -> [String] -> IO [Episode]
getSelectedEpisodes _ _ [] = return []
getSelectedEpisodes dbh pc ["all"] = getEpisodes dbh pc
getSelectedEpisodes dbh pc episodelist =
    do eps <- getEpisodes dbh pc
       return $ uniq . filter (\e -> (epid e `elem` eplist)) $ eps
    where eplist = map read episodelist

