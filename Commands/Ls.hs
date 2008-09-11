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

module Commands.Ls(lsrecent, lsfollowing, lsfollowers) where
import Utils
import System.Log.Logger
import Types
import Text.Printf
import System.Console.GetOpt
import Data.List
import Text.XML.HaXml
import Download
import FeedParser

i = infoM "ls"

stdopts = [Option "a" ["all"] (NoArg ("a", "")) 
                      "Show ALL results, not just 1st page\n\
                      \WARNING: may generate excessive traffic.\n\
                      \Use with caution!",
           Option "l" ["long"] (NoArg ("l", "")) 
                      "Long format output -- more info and\n\
                      \tab-separated columns"]

paginated workerfunc cppath cp (args, remainder)
    | lookup "a" args == Nothing = 
        do workerfunc cppath cp (args, remainder) 1
           return ()
    | otherwise = paginateloop 1
    where paginateloop page =
              do r <- workerfunc cppath cp (args, remainder) page
                 if null r
                     then return ()
                     else paginateloop (page + 1)

--------------------------------------------------
-- lsrecent
--------------------------------------------------

lsrecent = simpleCmd "lsrecent" "List recent updates from those you follow"
             lsrecent_help
             stdopts (paginated lsrecent_worker)

lsrecent_worker _ cp (args, _) page =
    do xmlstr <- sendAuthRequest cp "/statuses/friends_timeline.xml" 
                 [("page", show page)] []
       debugM "lsrecent" $ "Got doc: " ++ xmlstr
       handleStatus args xmlstr

lsrecent_help =
 "Usage: twidge lsrecent\n\n"

handleStatus args xmlstr = 
    let doc = getContent . xmlParse "lsrecent" . stripUnicodeBOM $ xmlstr
        statuses = map procStatuses . getStatuses $ doc
    in do mapM_ (printStatus args) statuses
          return statuses

procStatuses :: Content -> (String, String, String)
procStatuses item = 
    (sanitize $ contentToString 
      (keep /> tag "user" /> tag "screen_name" /> txt $ item),
     sanitize $ contentToString (keep /> tag "text" /> txt $ item),
     sanitize $ contentToString (keep /> tag "id" /> txt $ item)
    )

getStatuses = tag "statuses" /> tag "status"
printStatus args (name, text, updid) =
    case lookup "l" args of
      Nothing -> printf "<%s> %s\n" name text
      Just _ ->  printf "%s\t%s\t%s\n" updid name text

--------------------------------------------------
-- lsfollowing
--------------------------------------------------

lsfollowing = simpleCmd "lsfollowing" "List people you are following"
             lsfollowing_help
             stdopts (paginated lsfollowing_worker)

lsfollowing_worker _ cp (args, user) page =
    do xmlstr <- sendAuthRequest cp url [("page", show page)] []
       debugM "lsfollowing" $ "Got doc: " ++ xmlstr
       let doc = getContent . xmlParse "lsfollowing" . stripUnicodeBOM $ xmlstr
       let users = map procUsers . getUsers $ doc
       mapM_ (printUser args) users
       return users
       
    where url = case user of
                  [] -> "/statuses/friends.xml"
                  [x] -> "/statuses/friends/" ++ x ++ ".xml"
                  _ -> error "Invalid args to lsfollowing; see twidge lsfollowing --help"

printUser args (name, userid) = 
    case lookup "l" args of
      Nothing -> putStrLn name
      Just _ -> printf "%s\t%s\n" userid name

getUsers = tag "users" /> tag "user"

procUsers :: Content -> (String, String)
procUsers item =
    (sanitize $ contentToString (keep /> tag "screen_name" /> txt $ item),
     sanitize $ contentToString (keep /> tag "id" /> txt $ item))

lsfollowing_help =
 "Usage: twidge lsfollowing [username]\n\n\
 \If username is given, list the twitter accounts that user is following.\n\
 \Otherwise, list the twitter accounts your user is following.\n"

--------------------------------------------------
-- lsfollowers
--------------------------------------------------

lsfollowers = simpleCmd "lsfollowers" "List people that follow you"
             lsfollowers_help
             stdopts (paginated lsfollowers_worker)

lsfollowers_worker _ cp (args, user) page =
    do xmlstr <- sendAuthRequest cp url [("page", show page)] []
       debugM "lsfollowers" $ "Got doc: " ++ xmlstr
       let doc = getContent . xmlParse "lsfollowers" . stripUnicodeBOM $ xmlstr
       let users = map procUsers . getUsers $ doc
       mapM_ (printUser args) users
       return users
       
    where url = case user of
                  [] -> "/statuses/followers.xml"
                  [x] -> "/statuses/followers/" ++ x ++ ".xml"
                  _ -> error "Invalid args to lsfollowers; see twidge lsfollowers --help"

lsfollowers_help =
 "Usage: twidge lsfollowers [username]\n\n\
 \If username is given, list the twitter accounts that follow the user.\n\
 \Otherwise, list the twitter accounts that follow you.\n"
