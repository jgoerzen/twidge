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

module Commands.Ls(lsrecent, lsreplies, lsfollowing, lsfollowers, lsarchive, lsdm, lsdmarchive) where
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
import HSH
import System.Console.GetOpt.Utils

i = infoM "ls"

stdopts = [Option "a" ["all"] (NoArg ("a", "")) 
                      "Show ALL results, not just 1st page\n\
                      \WARNING: may generate excessive traffic.  \
                      \Use with caution!",
           Option "l" ["long"] (NoArg ("l", "")) 
                      "Long format output -- more info and \
                      \tab-separated columns"]

sinceopts = [
           Option "e" ["exec"] (ReqArg (stdRequired "e") "COMMAND")
                  "Suppress normal output, and instead call COMMAND\n\
                  \once for each output item.  The command will be\n\
                  \passed exactly four arguments: update ID,\n\
                  \username, suggested Message-ID, and update\n\
                  \content.  These arguments may contain shell\n\
                  \metacharacters.",
           Option "m" ["mailto"] (ReqArg (stdRequired "m") "ADDRESS")
                  "Suppress normal output, and instead generate an\n\
                  \email with the data and send it to ADDRESS.",
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
             (stdopts ++ sinceopts) (paginated (statuses_worker "lsrecent"
                                                "/statuses/friends_timeline"))

lsreplies = simpleCmd "lsreplies" "List recent replies to you"
            lsreplies_help
            (stdopts ++ sinceopts) (paginated (statuses_worker "lsreplies" 
                                               "/statuses/replies"))

lsarchive = simpleCmd "lsarchive" "List recent status updates you posted yourself"
            lsarchive_help
            (stdopts ++ sinceopts) (paginated (statuses_worker "lsarchive"
                                               "/statuses/user_timeline"))

lsdm = simpleCmd "lsdm" "List recent direct messages to you"
       lsdm_help
       (stdopts ++ sinceopts) (paginated (statuses_worker "lsdm" 
                                          "/direct_messages"))

lsdmarchive = simpleCmd "lsdm" "List recent direct messages you sent"
              lsdmarchive_help
              (stdopts ++ sinceopts) (paginated (statuses_worker "lsdmarchive"
                                                 "/direct_messages/sent"))

statuses_worker section command cpath cp (args, _) page =
    do xmlstr <- sendAuthRequest cp (command ++ ".xml")
                 (("page", show page) : sinceArgs section cp args)
                 []
       debugM section $ "Got doc: " ++ xmlstr
       results <- handleStatus section cp args xmlstr
       when (page == 1) $
            maybeSaveList section cpath cp args 
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

lsreplies_help =
 "Usage: twidge lsreplies [options]\n\n\
 \You can see the 20 most recent @replies from others to you with:\n\n\
 \   twidge lsreplies\n\n\
 \For more examples, including how to see only unseen replies, please\n\
 \refer to the examples under twidge lsrecent --help, which also pertain\n\
 \to lsreplies.\n"

lsarchive_help =
 "Usage: twidge lsarchive [options]\n\n\
 \You can see the 20 most recent updates you posted with:\n\n\
 \   twidge lsarchive\n\n\
 \For more examples, including how to see only unseen updates, please\n\
 \refer to the examples under twidge lsrecent --help, which also pertain\n\
 \to lsarchive.\n"

lsdm_help =
 "Usage: twidge lsdm [options]\n\n\
 \You can see the 20 most recent direct messages to you with:\n\n\
 \   twidge lsdm\n\n\
 \For more examples, including how to see only unseen updates, please\n\
 \refer to the examples under twidge lsrecent --help, which also pertain\n\
 \to lsdm.\n"
 
lsdmarchive_help =
 "Usage: twidge lsdmarchive [options]\n\n\
 \You can see the 20 most recent direct messages you sent with:\n\n\
 \   twidge lsdmarchive\n\n\
 \For more examples, including how to see only unseen updates, please\n\
 \refer to the examples under twidge lsrecent --help, which also pertain\n\
 \to lsdmarchive.\n"
 

handleStatus section cp args xmlstr = 
    let doc = getContent . xmlParse "lsrecent" . stripUnicodeBOM $ xmlstr
        statuses = map procStatuses . getStatuses $ doc
    in do mapM_ (printStatus section cp args) statuses
          return statuses

procStatuses :: Content -> (String, String, String)
procStatuses item = 
    (sanitize $ contentToString 
      (keep /> tag "user" /> tag "screen_name" /> txt $ item),
     sanitize $ contentToString (keep /> tag "text" /> txt $ item),
     sanitize $ contentToString (keep /> tag "id" /> txt $ item)
    )

getStatuses = tag "statuses" /> tag "status"
printStatus section cp args (name, text, updid) =
    case (lookup "m" args) of
      Nothing -> 
          case (lookup "e" args, lookup "l" args) of
            (Just cmd, _) ->
                runIO $ (cmd, [updid, name, msgid, text])
            (Nothing, Nothing) -> 
                do printf "%-22s %s\n" ("<" ++ name ++ ">") (head wrappedtext)
                   mapM_ (printf "%-22s %s\n" "") (tail wrappedtext)
            (Nothing, Just _) ->  printf "%s\t%s\t%s\n" updid name text
      Just recipient -> mailto section cp args (name, text, updid) recipient
    where wrappedtext = wrapText (80 - 22 - 2) (words text)
          msgid = genMsgId updid name cp

mailto section cp args (name, text, updid) recipient =
    runIO $ echo (message ++ "\n") -|- (sendmail, ["-t"])
    where sendmail = (forceEither $ get cp section "sendmail")::String
          msgid = genMsgId updid name cp
          subject = take 30 text ++ "... (twidge " ++ section ++ ")"
          message = unlines $ 
                    (case get cp section "mailfrom" of
                      Left _ -> ["Subject: " ++ name ++ ": " ++ subject]
                      Right x -> ["From: " ++ name ++ " <" ++ x ++ ">",
                                  "Subject: " ++ subject]
                    ) ++ 
                    ["Message-ID: " ++ msgid,
                     "X-Twidge-urlbase: " ++ forceEither (get cp "DEFAULT" "urlbase"),
                     "X-Twidge-server-base: " ++ serverHost cp,
                     "X-Twidge-command: " ++ section,
                     "X-Twidge-update-id: " ++ updid,
                     "X-Twidge-update-name: " ++ name,
                     "To: " ++ recipient,
                     "",
                     text,
                     "",
                     "(from " ++ name ++ ")"]

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
