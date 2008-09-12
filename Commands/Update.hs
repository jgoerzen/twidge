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

module Commands.Update(update) where
import Utils
import System.Log.Logger
import Types
import Text.Printf
import System.Console.GetOpt
import Data.List
import Text.XML.HaXml hiding (when)
import Download
import FeedParser
import Control.Monad(when)
import Text.Regex.Posix
import Data.ConfigFile

i = infoM "update"

update = simpleCmd "update" "Update your status"
             update_help
             [] update_worker

update_worker x cp ([], []) =
    do l <- getLine
       update_worker x cp ([], [l])
update_worker _ cp ([], [status]) =
    do when (length status > 140)
            (permFail $ "Your status update was " ++ show (length status) ++
                      " characters; max length 140")
       poststatus <- case get cp "update" "shortenurls" of
                       Right True -> shortenUrls status
                       _ -> return status
       xmlstr <- sendAuthRequest cp "/statuses/update.xml" [] 
                 [("source", "Twidge"), ("status", poststatus)]
       debugM "update" $ "Got doc: " ++ xmlstr
       
update_worker _ _ _ =
    permFail "update: syntax error; see twidge update --help"

shortenUrls "" = return ""
shortenUrls status = 
 do debugM "update" $ "shortenUrls considering: " ++ show status
    if match == ""
       then return before       -- No match means no "after"
       else do tiny <- mkTinyURL match
               debugM "update" $ "Got tinyurl: " ++ show tiny
               rest <- shortenUrls after
               return $ 
                      before ++ (if (length tiny < length match)
                                    then tiny else match)
                             ++ rest
    where (before, match, after) = status =~ pat
          pat = "(http|ftp)://[^ ]+"

mkTinyURL url = 
    simpleDownload $ "http://tinyurl.com/api-create.php?url=" ++ url

update_help =
 "Usage: twidge update [status]\n\n\
 \Updates your status to the given status.  You will most likely need to\n\
 \quote this to prevent interference from the shell.  For instance:\n\n\
 \  twidge update \"At home, baking.\"\n\n\
 \You can also omit the status, in which case a single line will be read\n\
 \from stdin and taken as your update.  Example:\n\n\
 \  date | twidge update\n"
