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
import Text.XML.HaXml hiding (when)
import Download
import FeedParser
import Data.ConfigFile
import Data.String.Utils(strip)
import Config
import Data.Either.Utils(forceEither)
import Control.Monad(when)
import Distribution.Simple.Utils(wrapText)

i = infoM "ls"

stdopts = [Option "a" ["all"] (NoArg ("a", "")) 
                      "Show ALL results, not just 1st page\n\
                      \WARNING: may generate excessive traffic.  \
                      \Use with caution!",
           Option "l" ["long"] (NoArg ("l", "")) 
                      "Long format output -- more info and \
                      \tab-separated columns"]

sinceopts = [
           Option "s" ["saveid"] (NoArg ("s", ""))
                      "Save topmost ID for future use with --unseen.\n\
                      \Will write the ID to your config file.",
           Option "u" ["unseen"] (NoArg ("u", ""))
                      "Show only items since the last use of --saveid"]

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

maybeSaveList section cpath cp args [] = return ()
maybeSaveList section cpath cp args newids =
    do debugM section $ "maybeSaveList called: " ++ show args ++ " " ++ show newids
       maybeSave section cpath cp args theid
    where theid = maximum . map (read::String -> Integer) $ newids

maybeSave section cpath cp args newid =
    case (lookup "s" args, get cp section "lastid") of
      (Nothing, _) -> return ()
      (_, Left _) -> saveid
      (_, Right x) ->
          if (read x) < (newid::Integer)
             then return ()
             else saveid
    where saveid = writeCP cpath newcp
          newcp = forceEither $
                  do cp2 <- if (has_section cp section)
                                then return cp
                                else add_section cp section
                     cp2 <- set cp2 section "lastid" (show newid)
                     return cp2

sinceArgs section cp args =
    case (lookup "u" args, get cp section "lastid") of
      (Nothing, _) -> []
      (_, Left _) -> []
      (_, Right a) -> 
          [("since_id", strip a)]

--------------------------------------------------
-- lsrecent
--------------------------------------------------

lsrecent = simpleCmd "lsrecent" "List recent updates from those you follow"
             lsrecent_help
             (stdopts ++ sinceopts) (paginated lsrecent_worker)

lsrecent_worker cpath cp (args, _) page =
    do xmlstr <- sendAuthRequest cp "/statuses/friends_timeline.xml" 
                 (("page", show page) : sinceArgs "lsrecent" cp args)
                 []
       debugM "lsrecent" $ "Got doc: " ++ xmlstr
       results <- handleStatus args xmlstr
       when (page == 1) $
            maybeSaveList "lsrecent" cpath cp args 
                          ((map (\(_, _, i) -> i)) results)
       return results

lsrecent_help =
 "Usage: twidge lsrecent [options]\n\n\
 \You can see the 20 most recent items from your friends with:\n\n\
 \   twidge lsrecent\n\n\
 \To see items that you haven't seen yet, and remember this for the future,\n\
 \use:\n\n\
 \   twidge lsrecent -su\n\n\
 \After running that once, you may want to use -asu in the future to get all\n\
 \unseen messages, even if there are more than 20.  Don't use -a until\n\
 \you've used -s at least once.\n"

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
      Nothing -> 
          do printf "%-22s %s\n" ("<" ++ name ++ ">") (head wrappedtext)
             mapM_ (printf "%-22s %s\n" "") (tail wrappedtext)
      Just _ ->  printf "%s\t%s\t%s\n" updid name text
    where wrappedtext = wrapText (80 - 22 - 2) (words text)

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
 "Usage: twidge lsfollowing [options] [username]\n\n\
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
 "Usage: twidge lsfollowers [options] [username]\n\n\
 \If username is given, list the twitter accounts that follow the user.\n\
 \Otherwise, list the twitter accounts that follow you.\n"
