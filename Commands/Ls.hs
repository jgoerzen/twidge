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

module Commands.Ls(lsrecent, lsreplies, lsblocking,
                   lsfollowing, lsfollowers, lsarchive, 
                   lsrt, lsrtreplies, lsrtarchive, lsdm, lsdmarchive) where
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
import HSH
import System.Console.GetOpt.Utils
import Network.URI
import Data.Maybe (isJust)
import Network.OAuth.Http.Request

i = infoM "ls"

defaultWidth = 80

stdopts = [Option "a" ["all"] (NoArg ("a", "")) 
                      "Show ALL results, not just 1st page\n\
                      \WARNING: may generate excessive traffic.  \
                      \Use with caution!",
           Option "l" ["long"] (NoArg ("l", "")) 
                      "Long format output -- more info and \
                      \tab-separated columns",
           Option "w" ["width"] (ReqArg (stdRequired "w") "WIDTH")
                      ("Set the margin at which word-wrapping occurs.\n\
                       \Ignored in long format mode. Default is "
                       ++ show defaultWidth ++ ".")]

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

maybeSaveList section cpath cp args [] = return ()
maybeSaveList section cpath cp args newids =
    do debugM section $ "maybeSaveList called for " ++ section ++ ": " 
                  ++ show args ++ " " ++ show newids
       maybeSave section cpath cp args theid
    where theid = maximum . map (read::String -> Integer) $ newids

maybeSave section cpath cp args newid =
    let sArg = isJust $ lookup "s" args
        sConf = isRight True $ get cp "DEFAULT" "savelast"
    in case (any id [sArg,sConf], get cp section "lastid") of
      (False, _)     -> do debugM "maybeSave" "maybeSave: No -s nor savelast"
                           return ()
      (True, Left _) -> do debugM "maybeSave" "maybeSave: Will add ID"
                           saveid
      (True, Right x) ->
          if (read x) > (newid::Integer)
             then return ()
             else saveid
    where saveid = writeCP cpath newcp
          newcp = forceEither $
                  do cp2 <- if (has_section cp section)
                                then return cp
                                else add_section cp section
                     cp2 <- set cp2 section "lastid" (show newid)
                     return cp2
          isRight _  (Left _)   = False
          isRight v1 (Right v2) = v1 == v2

sinceArgs section cp args =
    case (lookup "u" args, get cp section "lastid") of
      (Nothing, _) -> []
      (_, Left _) -> []
      (_, Right a) -> 
          [("since_id", strip a)]

screenNameArgs args =
  case lookup "U" args of
    Nothing -> []
    Just username -> [("screen_name", username)]

--------------------------------------------------
-- lsrecent & friends
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
            (stdopts ++ sinceopts ++ usernameopts) 
            (paginated (statuses_worker "lsarchive"
                        "/statuses/user_timeline"))
              where 
                usernameopts = [
                  Option "U" ["username"] (ReqArg (stdRequired "U") "USERNAME")
                  "Instead of showing your own updates, show\n\
                  \those of the given username."
                  ]




lsdm = simpleCmd "lsdm" "List recent direct messages to you"
       lsdm_help
       (stdopts ++ sinceopts) (paginated (dm_worker "lsdm" 
                                          "/direct_messages"))

lsdmarchive = simpleCmd "lsdmarchive" "List recent direct messages you sent"
              lsdmarchive_help
              (stdopts ++ sinceopts) (paginated (dm_worker "lsdmarchive"
                                                 "/direct_messages/sent"))

lsrt = simpleCmd "lsrt" "List recent retweets from those you follow"
       lsrt_help
       (stdopts ++ sinceopts) (paginated (statuses_worker "lsrt"
                                          "/statuses/retweeted_to_me"))
       
lsrtarchive = simpleCmd "lsrtarchive" "List recent retweets you made yourself"
              lsrtarchive_help
              (stdopts ++ sinceopts) (paginated (statuses_worker "lsrtarchive"
                                                 "/statuses/retweeted_by_me"))

lsrtreplies = simpleCmd "lsrtreplies" "List others' retweets of your statuses"
              lsrtreplies_help
              (stdopts ++ sinceopts) (paginated (statuses_worker "lsrtreplies"
                                                 "/statuses/retweets_of_me"))

statuses_worker = generic_worker handleStatus
dm_worker = generic_worker handleDM

generic_worker procfunc section command cpath cp (args, _) page =
    do xmlstr <- sendAuthRequest GET cp (command ++ ".xml")
                 (("page", show page) : sinceArgs section cp args
                  ++ screenNameArgs args)
                 []
       debugM section $ "Got doc: " ++ xmlstr
       results <- procfunc section cp args xmlstr
       when (page == 1) $
            maybeSaveList section cpath cp args (map sId results)
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

lsrt_help = 
 "Usage: twidge lsrt [options]\n\n\
 \You can see the 20 most recent retweets posted by those you follow with:\n\n\
 \   twidge lsrt\n\n\
 \For more examples, including how to see only unseen retweets, please\n\
 \refer to the examples under twidge lsrecent --help, which also pertain\n\
 \to lsreplies.\n"
 
lsrtreplies_help = 
 "Usage: twidge lsrtreplies [options]\n\n\
 \You can see the 20 most retweets made of your statuses with:\n\n\
 \   twidge lsrtreplies\n\n\
 \For more examples, including how to see only unseen retweets, please\n\
 \refer to the examples under twidge lsrecent --help, which also pertain\n\
 \to lsreplies.\n"

lsrtarchive_help = 
 "Usage: twidge lsrt [options]\n\n\
 \You can see the 20 most recent retweets you made:\n\n\
 \   twidge lsrtarchive\n\n\
 \For more examples, including how to see only unseen retweets, please\n\
 \refer to the examples under twidge lsrecent --help, which also pertain\n\
 \to lsreplies.\n"


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

handleStatus = handleGeneric (map procStatuses . getStatuses) printStatus
handleDM = handleGeneric (map procDM . getDMs) printDM

handleGeneric pfunc printfunc section cp args xmlstr = 
    let doc = getContent . xmlParse section . stripUnicodeBOM $ xmlstr
        statuses = pfunc doc
    in do mapM_ (printfunc section cp args) statuses
          return statuses

procStatuses :: Content -> Message
procStatuses item = 
    Message {sId = s (tag "id") item,
             sSender = s (tag "user" /> tag "screen_name") item,
             sRecipient = "",
             sText = unEsc $ s (tag "text") item,
             sDate = s (tag "created_at") item}

s f item = sanitize $ contentToString (keep /> f /> txt $ item)

procDM :: Content -> Message
procDM item =
    Message {sId = s (tag "id") item,
             sSender = s (tag "sender_screen_name") item,
             sRecipient = s (tag "recipient_screen_name") item,
             sText = unEsc $ s (tag "text") item,
             sDate = s (tag "created_at") item}

getStatuses = tag "statuses" /> tag "status"
getDMs = tag "direct-messages" /> tag "direct_message"

longStatus :: Message -> String
longStatus m = printf "%s\t%s\t%s\t%s\t%s\t\n"
               (sId m) (sSender m) (sRecipient m) (sText m) (sDate m)
shortStatus :: Int -> Message -> String
shortStatus width m = 
    (printf "%-22s %s\n" ("<" ++ sSender m ++ ">")
               (head wrappedtext)) ++
    concatMap (printf "%-22s %s\n" "") (tail wrappedtext)
    where wrappedtext = map unwords $ wrapLine (width - 22 - 2) (words (sText m))

shortDM :: Int -> Message -> String
shortDM width m =
     (printf "%-22s %-22s %s\n" ("<" ++ sSender m ++ ">")
                                 ("<" ++ sRecipient m ++ ">")
                                 (head wrappedtext)) ++
     concatMap (printf "%-22s %-22s %s\n" "" "") (tail wrappedtext)
     where wrappedtext = map unwords $ wrapLine (width - 22 - 22 - 3) (words (sText m))

printStatus section cp args m = 
    printGeneric shortStatus longStatus section cp args m

printDM section cp args m =
    printGeneric shortDM longStatus section cp args m

printGeneric shortfunc longfunc section cp args m =
    case (lookup "m" args) of
      Nothing -> 
          case (lookup "e" args, lookup "l" args) of
            (Just cmd, _) ->
                runIO $ (cmd, [sId m, sSender m, sRecipient m, 
                               sText m, sDate m])
            (Nothing, Nothing) -> putStr (shortfunc (case lookup "w" args of 
                                                            Just ws -> read ws
                                                            Nothing -> defaultWidth) m)
            (Nothing, Just _) -> putStr (longfunc m)
      Just recipient -> mailto section cp args m recipient
    where msgid = genMsgId section m cp

mailto section cp args m recipient =
    runIO $ echo (message ++ "\n") -|- (sendmail, ["-t"])
    where sendmail = (forceEither $ get cp section "sendmail")::String
          msgid = genMsgId section m cp
          subject = take 30 (sText m) ++ "... (twidge " ++ section ++ ")"
          message = unlines $ 
                    (case get cp section "mailfrom" of
                      Left _ -> ["Subject: " ++ (sSender m) ++ ": " ++ subject]
                      Right x -> ["From: " ++ (sSender m) ++ " <" ++ x ++ ">",
                                  "Subject: " ++ subject]
                    ) ++ 
                    ["Message-ID: " ++ msgid,
                     "X-Twidge-urlbase: " ++ forceEither (get cp "DEFAULT" "urlbase"),
                     "X-Twidge-server-base: " ++ serverHost cp,
                     "X-Twidge-command: " ++ section,
                     "X-Twidge-update-id: " ++ sId m,
                     "X-Twidge-update-sender: " ++ sSender m,
                     "X-Twidge-update-recipient: " ++ sRecipient m,
                     "To: " ++ recipient,
                     "",
                     sText m,
                     "",
                     "(from " ++ sSender m ++ ")"
                    ,""
                    ,"Tweet URL: http://twitter.com/" ++ sSender m ++
                     "/status/" ++ sId m
                    ,"Reply URL: http://twitter.com/home?status=@" ++
                     escapeURIString isUnreserved (sSender m ++ " ") ++
                     "&in_reply_to_status_id=" ++ sId m ++ "&in_reply_to=" ++
                     escapeURIString isUnreserved (sSender m)
                    ,"User home: http://twitter.com/" ++ sSender m
                    ]

----------------------------------------------------------------------
-- Follow/block type commands
----------------------------------------------------------------------

--------------------------------------------------
-- lsfollowing
--------------------------------------------------

lsfollowing = simpleCmd "lsfollowing" "List people you are following"
             lsfollowing_help
             stdopts (paginated lsfollowing_worker)
lsfollowing_worker = genericfb_worker "lsfollowing" "/statuses/friends"
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
lsfollowers_worker = genericfb_worker "lsfollowers" "/statuses/followers"
lsfollowers_help =
 "Usage: twidge lsfollowers [options] [username]\n\n\
 \If username is given, list the twitter accounts that follow the user.\n\
 \Otherwise, list the twitter accounts that follow you.\n"

--------------------------------------------------
-- lsblocking
--------------------------------------------------

lsblocking = simpleCmd "lsblocking" "List people you are blocking"
             lsblocking_help
             stdopts (paginated lsblocking_worker)
lsblocking_worker = genericfb_worker "lsblocking" "/blocks/blocking"
lsblocking_help =
 "Usage: twidge lsblocking [options]\n\n\
 \List the twitter accounts that your account is blocking.\n"

------------------------------------------------------------
-- Generic follow/block support
-- urlbase should be "/statuses/followers" or "/statuses/friends"
------------------------------------------------------------

genericfb_worker cmdname urlbase _ cp (args, user) page =
    do xmlstr <- sendAuthRequest GET cp url [("page", show page)] []
       debugM cmdname $ "Got doc: " ++ xmlstr
       let doc = getContent . xmlParse cmdname . stripUnicodeBOM $ xmlstr
       let users = map procUsers . getUsers $ doc
       mapM_ (printUser args) users
       return users
       
    where url = case user of
                  [] -> urlbase ++ ".xml"
                  [x] -> urlbase ++ "/" ++ x ++ ".xml"
                  _ -> error $ "Invalid args to " ++ cmdname ++ 
                       "; see twidge " ++ cmdname ++ " --help"
          printUser args (name, userid) = 
              case lookup "l" args of
                Nothing -> putStrLn name
                Just _ -> printf "%s\t%s\n" userid name

          getUsers = tag "users" /> tag "user"

          procUsers :: Content -> (String, String)
          procUsers item =
              (sanitize $ contentToString (keep /> tag "screen_name" /> txt $ item),
               sanitize $ contentToString (keep /> tag "id" /> txt $ item))

----------------------------------------------------------------------
-- Generic Utilities
----------------------------------------------------------------------

{- | Calls the workerfunc once if --all wasn't given, to load page 1.

Otherwise, calls workerfunc repeatedly to load all pages, one after another, as
long as it continues to return data.

Used to wrap around worker functions where multiple pages of data can be
returned. -}
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

{- | Twitter has an additional level of escaping for &lt; and &gt; only. 
Sigh. -}
unEsc :: String -> String
unEsc [] = []
unEsc x 
  | "&lt;" `isPrefixOf` x = '<' : unEsc (drop 4 x)
  | "&gt;" `isPrefixOf` x = '>' : unEsc (drop 4 x)
  | otherwise = head x : unEsc (tail x)
