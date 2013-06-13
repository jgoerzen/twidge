{-# LANGUAGE CPP #-}
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

module Commands.Update(update, dmsend) where
import Utils
import System.Log.Logger
import Types
import System.Console.GetOpt
import System.Console.GetOpt.Utils
import Download
import Control.Monad(when)
import Text.Regex.Posix
import Data.ConfigFile
import MailParser(message)
import Text.ParserCombinators.Parsec
import Network.OAuth.Http.Request
#ifdef USE_BITLY
import Network.Bitly (Account(..),bitlyAccount,jmpAccount,shorten)
#endif
import qualified Codec.Binary.UTF8.String as UTF8

i = infoM "update"
d = debugM "update"

update = simpleCmd "update" "Update your status"
             update_help
             [Option "r" ["recvmail"] (NoArg ("m", "")) 
              "Receive update as body of email on stdin",
              Option "i" ["inreplyto"] (ReqArg (stdRequired "i") "MSGID")
              "Indicate this message is in reply to MSGID"
             ]
             update_worker_wrapper

update_worker_wrapper x cp args =
  do d $ "Running update_worker with: " ++ show (x, args)
     update_worker x cp (newargs args)
  where newargs (opts, status) = (opts, map UTF8.decodeString status)

update_worker x cp ([("m", "")], []) =
    do d "Reading mail message"
       c <- getContents
       case parse message "(stdin)" c of
         Left x -> permFail $ "Couldn't parse mail: " ++ show x
         Right (refs, body) ->
             let irt = case refs =~ "<([^>]+)>$" of
                         "" -> []
                         m -> case parseMsgId m of
                                Nothing -> []
                                Just (m, host, section) ->
                                    if host == serverHost cp && 
                                       section `elem` ["lsrecent", "lsarchive",
                                                       "lsreplies"]
                                       then [("in_reply_to_status_id",
                                              sId m)]
                                       else []
                 status = body
             in do poststatus <- procStatus cp "update" status
                   json <- sendAuthRequest POST cp "/statuses/update.json" []
                             ([("status", poststatus)] ++ irt)
                   debugM "update" $ "Got doc: " ++ show json

update_worker x cp ([], []) =
    do d "No args reading line"
       l <- getLine
       update_worker x cp ([], [l])

update_worker x cp ([("i", id )], []) =
    do d "-i reading line"
       l <- getLine
       update_worker x cp ([("i", id)], [l])

update_worker _ cp ([("i", id)], [status]) =
    do d "-i have line"
       poststatus <- procStatus cp "update" status
       json <- sendAuthRequest POST cp "/statuses/update.json" []
                 [("status", poststatus), ("in_reply_to_status_id", id)]
       debugM "update" $ "Got doc: " ++ show json

update_worker _ cp ([], [status]) =
    do d "no args have line"
       poststatus <- procStatus cp "update" status
       json <- sendAuthRequest POST cp "/statuses/update.json" []
                 [("status", poststatus)]
       debugM "update" $ "Got doc: " ++ show json
update_worker _ _ _ =
    permFail "update: syntax error; see twidge update --help"

procStatus cp section status =
    do poststatus <- case get cp section "shortenurls" of
                       Right True -> case get cp section "shortenall" of
                                       Right True -> shortenUrls cp status
                                       _ | length status > 140
                                                  -> shortenUrls cp status
                                       _          -> return status
                       _          -> return status
       when (length poststatus > 140)
                (permFail $ "Your status update was " ++ 
                          show (length poststatus) ++
                          " characters; max length 140")
       return poststatus

dmsend = simpleCmd "dmsend" "Send direct message"
         dmsend_help
         []
         dmsend_worker

dmsend_worker x cp ([], [r]) =
    do l <- getLine
       dmsend_worker x cp ([], [r, l])
dmsend_worker x cp ([], [recipient, status]) =
    do poststatus <- procStatus cp "dmsend" status
       json <- sendAuthRequest POST cp "/direct_messages/new.json" []
                 [("text", poststatus), ("screen_name", recipient)]
       debugM "dmsend" $ "Got doc: " ++ show json
dmsend_worker _ _ _ = permFail "Syntax error; see twidge dmsend --help"

shortenUrls _ "" = return ""
shortenUrls cp status =
 do debugM "update" $ "shortenUrls considering: " ++ show status
    shortURL <- chooseShortener cp
    if match == ""
       then return before       -- No match means no "after"
       else do tiny <- shortURL match
               debugM "update" $ "Got tinyurl: " ++ show tiny
               rest <- shortenUrls cp after
               return $ 
                      before ++ (if (length tiny < length match)
                                    then tiny else match)
                             ++ rest
    where (before, match, after) = status =~ pat
          pat = "(http|https|ftp)\\://[a-zA-Z0-9\\-\\.]+(:[a-zA-Z0-9]*)?/?([-a-zA-Z0-9:()\\._\\?\\,\\'/\\\\\\+&%\\$#\\=~])*"

#ifdef USE_BITLY
chooseShortener cp = do
  -- look either for [bitly] or [jmp] section in config
  let (sec, newAccount) = if has_section cp "bitly"
         then ("bitly", bitlyAccount)
         else ("jmp",   jmpAccount)
  -- [bitly] or [jmp] section should define both login and apikey
  let acc = get cp sec "login" >>= \l ->
            get cp sec "apikey" >>= \k ->
            return $ newAccount { login=l, apikey=k }
  return $ case acc of
    Left _  -> mkTinyURL    -- use default
    Right a -> mkBitlyURL a

mkBitlyURL acc url = do
  r <- shorten acc url
  case r of
    Left  e        -> permFail e      -- report bit.ly errors
    Right shorturl -> return shorturl
#else
chooseShortener _ = return mkTinyURL
#endif

mkTinyURL url = 
    simpleDownload . concat $ "http://is.gd/api.php?longurl=" : map escapeHashes url
    where
      -- NOTE: This technique works with the is.gd "API"
      -- but does not work with the tinyurl.com "API"
      escapeHashes :: Char -> String
      escapeHashes '#' = "%23"
      escapeHashes c   = [c]

update_help =
 "Usage: twidge update [status]\n\n" ++
 "Updates your status to the given status.  You will most likely need to\n" ++
 "quote this to prevent interference from the shell.  For instance:\n\n" ++
 "  twidge update \"At home, baking.\"\n\n" ++
 "You can also omit the status, in which case a single line will be read\n" ++
 "from stdin and taken as your update.  Example:\n\n" ++
 "  date | twidge update\n"

dmsend_help =
 "Usage: twidge dmsend recipient [status]\n\n" ++
 "Sends a direct message to the given recipient.  You will most likely need\n" ++
 "to quote this to prevent interference from the shell.  For instance:\n\n" ++
 "  twidge dmsend unixtwidge \"At home, baking.\"\n\n" ++
 "You can also omit the status, in which case a single line will be read\n" ++
 "from stdin and taken as your update.  Example:\n\n" ++
 "  date | twidge dmsend unixtwidge\n"
