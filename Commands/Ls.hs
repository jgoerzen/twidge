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
                   lsrtreplies, status, lsdm, lsdmarchive) where
import Utils
import System.Log.Logger
import Types
import Text.Printf
import System.Console.GetOpt
import Download
import Data.ConfigFile
import Data.String.Utils(strip)
import Config
import Data.Either.Utils(forceEither)
import Control.Monad(when)
import HSH
import System.Console.GetOpt.Utils
import Network.URI
import Data.Maybe (isJust, isNothing)
import Network.OAuth.Http.Request
import Data.Time.Format (formatTime, parseTime)
import Data.Time.LocalTime (ZonedTime)
import System.Locale (defaultTimeLocale, rfc822DateFormat)

i = infoM "ls"

defaultWidth = 80

stdopts = [Option "a" ["all"] (NoArg ("a", "")) 
                      "Show ALL results, not just 1st page\n\
                      \WARNING: may generate excessive traffic.  \
                      \Use with caution!",
           Option "l" ["long"] (NoArg ("l", "")) 
                      "Long format output -- more info and \
                      \tab-separated columns",
           widthopt]

widthopt = Option "w" ["width"] (ReqArg (stdRequired "w") "WIDTH")
                      ("Set the margin at which word-wrapping occurs.\n\
                       \Ignored in long format mode. Default is "
                       ++ show defaultWidth ++ ".")

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

sinceArgs section cp args maxId = from ++ since where
    from = maybe [] (\i -> [("max_id", show i)]) maxId
    since = case (lookup "u" args, get cp section "lastid") of
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

lsrecent = simpleCmd "lsrecent" "List recent updates from your home timeline"
             lsrecent_help
             (stdopts ++ sinceopts) (paginated (statuses_worker "lsrecent"
                                                "/statuses/home_timeline"))

lsreplies = simpleCmd "lsreplies" "List recent messages mentioning you"
            lsreplies_help
            (stdopts ++ sinceopts) (paginated (statuses_worker "lsreplies" 
                                               "/statuses/mentions_timeline"))

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

status = simpleCmd "status" "Retrieve a single status"
         status_help
         [widthopt] status_worker

lsdm = simpleCmd "lsdm" "List recent direct messages to you"
       lsdm_help
       (stdopts ++ sinceopts) (paginated (dm_worker "lsdm" 
                                          "/direct_messages"))

lsdmarchive = simpleCmd "lsdmarchive" "List recent direct messages you sent"
              lsdmarchive_help
              (stdopts ++ sinceopts) (paginated (dm_worker "lsdmarchive"
                                                 "/direct_messages/sent"))

lsrtreplies = simpleCmd "lsrtreplies" "List others' retweets of your statuses"
              lsrtreplies_help
              (stdopts ++ sinceopts) (paginated (statuses_worker "lsrtreplies"
                                                 "/statuses/retweets_of_me"))

statuses_worker = generic_worker handleStatuses
dm_worker = generic_worker handleDM

status_worker _ cp (args, [statusId]) = do
  json <- sendAuthRequest GET cp "/statuses/show.json"
            [("id", statusId)] []
  debugM "status" $ "Got doc: " ++ show json
  let TimelineMessage status = decode_json json
  printStatus "status" cp args status
status_worker _ _ _ = error "Invalid args to status; see twidge status --help"


handleStatuses = handleGeneric (map fromTimeline) printStatus

generic_worker procfunc section command cpath cp (args, _) maxId = do
  json <- sendAuthRequest GET cp (command ++ ".json")
            (sinceArgs section cp args maxId
             ++ screenNameArgs args) []
  debugM section $ "Got doc: " ++ show json
  results <- procfunc section cp args json
  when (isNothing maxId) $
       maybeSaveList section cpath cp args (map sId results)
  return (results, nextMaxId results)

nextMaxId :: [Message] -> Maybe Integer
nextMaxId [] = Nothing
nextMaxId r = Just ((minimum $ map (read . sId) r) - 1)

status_help =
 "Usage: twidge status [options] status-id\n"

lsrecent_help =
 "Usage: twidge lsrecent [options]\n\n\
 \You can see the 20 most recent items from your timeline with:\n\n\
 \   twidge lsrecent\n\n\
 \To see items that you haven't seen yet, and remember this for the future,\n\
 \use:\n\n\
 \   twidge lsrecent -su\n\n\
 \After running that once, you may want to use -asu in the future to get all\n\
 \unseen messages, even if there are more than 20.  Don't use -a until\n\
 \you've used -s at least once.\n"

lsreplies_help =
 "Usage: twidge lsreplies [options]\n\n\
 \You can see the 20 most recent @mentions from others of you with:\n\n\
 \   twidge lsreplies\n\n\
 \For more examples, including how to see only unseen mentions, please\n\
 \refer to the examples under twidge lsrecent --help, which also pertain\n\
 \to lsreplies.\n"

lsarchive_help =
 "Usage: twidge lsarchive [options]\n\n\
 \You can see the 20 most recent updates you posted with:\n\n\
 \   twidge lsarchive\n\n\
 \For more examples, including how to see only unseen updates, please\n\
 \refer to the examples under twidge lsrecent --help, which also pertain\n\
 \to lsarchive.\n"

lsrtreplies_help = 
 "Usage: twidge lsrtreplies [options]\n\n\
 \You can see the 20 most retweets made of your statuses with:\n\n\
 \   twidge lsrtreplies\n\n\
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

handleDM = handleGeneric (map fromDM) printDM

handleGeneric pfunc printfunc section cp args json =
    let statuses = pfunc $ decode_json json
    in do mapM_ (printfunc section cp args) statuses
          return statuses

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
                    (case twitterToRFC822 (sDate m) of
                      Just d -> [ "Date: " ++ d ]
                      Nothing -> []
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
          twitterToRFC822 d =
            formatTime defaultTimeLocale rfc822DateFormat `fmap` time
              where
                time :: Maybe ZonedTime
                time = parseTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y" d

----------------------------------------------------------------------
-- Follow/block type commands
----------------------------------------------------------------------

--------------------------------------------------
-- lsfollowing
--------------------------------------------------

lsfollowing = simpleCmd "lsfollowing" "List people you are following"
             lsfollowing_help
             stdopts (paginated lsfollowing_worker)
lsfollowing_worker = genericfb_worker "lsfollowing" "/friends/list.json"
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
lsfollowers_worker = genericfb_worker "lsfollowers" "/followers/list.json"
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
lsblocking_worker = genericfb_worker "lsblocking" "/blocks/list.json"
lsblocking_help =
 "Usage: twidge lsblocking [options]\n\n\
 \List the twitter accounts that your account is blocking.\n"

------------------------------------------------------------
-- Generic follow/block support
------------------------------------------------------------

genericfb_worker cmdname url _ cp (args, user) page =
    do json <- sendAuthRequest GET cp url params []
       debugM cmdname $ "Got doc: " ++ show json
       let UserList doc nextPage = decode_json json
       let users = map fromListedUser doc
       mapM_ (printUser args) users
       return (users, nextPage)
    where params = pageParams ++ targetParams
          pageParams = maybe [] (\c -> [("cursor", c)]) page
          targetParams = case user of
                  [] -> []
                  [x] -> [("screen_name", x)]
                  _ -> error $ "Invalid args to " ++ cmdname ++ 
                       "; see twidge " ++ cmdname ++ " --help"
          printUser args (name, userid) = 
              case lookup "l" args of
                Nothing -> putStrLn name
                Just _ -> printf "%s\t%s\n" userid name

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
        do workerfunc cppath cp (args, remainder) Nothing
           return ()
    | otherwise = paginateloop Nothing
    where paginateloop page =
              do (r, nextPage) <- workerfunc cppath cp (args, remainder) page
                 if null r
                     then return ()
                     else paginateloop nextPage
