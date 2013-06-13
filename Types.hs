{-# LANGUAGE OverloadedStrings, CPP #-}
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
   Module     : Types
   Copyright  : Copyright (C) 2006-2007 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}
module Types where
import Control.Applicative
import Control.Monad (mzero)
import Data.ConfigFile
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.List (isPrefixOf)
import Data.String.Utils (strip)
import Data.Text (unpack)

decode_json :: FromJSON a => ByteString -> a
decode_json = either error id . eitherDecode

{- | Removes potentially problematic or malicious stuff -}
sanitize :: String -> String
sanitize = strip . map sanitizer
    where sanitizer c
              | c `elem` "\n\r\0\t" = ' '
              | otherwise = c

{- | Twitter has an additional level of escaping for &lt; and &gt; only.
Sigh. -}
unEsc :: String -> String
unEsc [] = []
unEsc x
  | "&lt;" `isPrefixOf` x = '<' : unEsc (drop 4 x)
  | "&gt;" `isPrefixOf` x = '>' : unEsc (drop 4 x)
  | otherwise = head x : unEsc (tail x)

data Command =
    Command {cmdname :: String,
             cmddescrip :: String,
             execcmd :: [String] -> Maybe FilePath -> ConfigParser -> IO ()}

data Message = Message {
      sId :: String,
      sSender :: String,
      sRecipient :: String,
      sText :: String,
      sDate :: String
    } deriving (Eq, Read, Show, Ord)


newtype TimelineMessage = TimelineMessage { fromTimeline :: Message }

instance FromJSON TimelineMessage where
  parseJSON j = TimelineMessage <$> parseTimelineMessage j

parseTimelineMessage (Object v) = Message <$>
    s v "id_str" <*>
    (v .: "user" >>= extractScreenName) <*>
    pure "" <*>
    retweetOrText v <*>
    s v "created_at"
parseTimelineMessage _ = mzero

newtype DirectMessage = DirectMessage { fromDM :: Message }

instance FromJSON DirectMessage where
  parseJSON j = DirectMessage <$> parseDirectMessage j

parseDirectMessage (Object v) = Message <$>
    s v "id_str" <*>
    s v "sender_screen_name" <*>
    s v "recipient_screen_name" <*>
    (unEsc <$> s v "text") <*>
    s v "created_at"
parseDirectMessage _ = mzero

extractScreenName (Object v) = s v "screen_name"
extractScreenName _ = mzero

retweetOrText v = unEsc <$> ((retweet v) <|> (unpack <$> v .: "text")) where
  retweet v = do
    rt <- v .: "retweeted_status"
    user <- rt .: "user" >>= extractScreenName
    text <- rt .: "text"
    return $ "RT @" ++ user ++ ": " ++ text

s v name = sanitize <$> v .: name

data UserList = UserList [ListedUser] (Maybe String)

newtype ListedUser = ListedUser { fromListedUser :: (String, String) }

instance FromJSON UserList where
  parseJSON (Object v) = UserList <$> v .: "users" <*> v .:? "next_cursor_str"
  parseJSON _ = mzero

instance FromJSON ListedUser where
  parseJSON (Object v) = (ListedUser .) . (,) <$>
    v .: "screen_name" <*>
    v .: "id_str"
  parseJSON _ = mzero
