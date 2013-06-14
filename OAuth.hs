{- hpodder component
Copyright (C) 2010 John Goerzen <jgoerzen@complete.org>

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
   Module     : OAuth
   Copyright  : Copyright (C) 2010 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}
module OAuth where

import Network.OAuth.Consumer
import Data.Maybe
import Network.OAuth.Http.Request
import Network.OAuth.Http.HttpClient
import Data.ConfigFile
import Utils
import Data.Either.Utils

obfuscatedTwitterKeys = ("jRlf9pXnU7uEV5ZcxrJmc",
               "L17bXFykkLYz4TBCmet1wuX9VtXbLq8Xj4Lif42O4ew")
identicaKeys = ("f027d666f9d0e7b80beaed528aec473c",
                "d84c9b3dafb14becb5e05a002886b60c")
twitterKeys = (twitterDeObfuscator (fst obfuscatedTwitterKeys),
               twitterDeObfuscator (snd obfuscatedTwitterKeys))

twitterDeObfuscator :: String -> String
twitterDeObfuscator = reverse . map rot13

instance Show Application where
  show (Application a b c) = "Application " ++ show a ++ " " ++ show b ++ " " ++ show c

instance Show Token where
  show (TwoLegg app oauthp) = "TwoLegg " ++ show app ++ " " ++ show oauthp
  show (ReqToken app oauthp) = "ReqToken " ++ show app ++ " " ++ show oauthp
  show (AccessToken app oauthp) = "AccessToken " ++ show app ++ " " ++ show oauthp

rot13 :: Char -> Char
rot13 x = 
    case lookup x trans of
        Just y -> y
        Nothing -> x
    where trans = zip (lcbase ++ ucbase) (part lcbase ++ part ucbase)
          lcbase = ['a' .. 'z']
          ucbase = ['A' .. 'Z']
          part x = drop 13 x ++ take 13 x

getDefaultKeys :: ConfigParser -> Maybe (String, String)
getDefaultKeys cp =
  case serverHost cp of
    "api.twitter.com" -> Just twitterKeys
    "twitter.com" -> Just twitterKeys
    "identi.ca" -> Just identicaKeys

getApp :: ConfigParser -> Maybe Application
getApp cp = 
  if (has_option cp "DEFAULT" "oauthconsumerkey" &&
      has_option cp "DEFAULT" "oauthconsumersecret")
  then Just $ Application 
                    {consKey = 
                         forceEither $ get cp "DEFAULT" "oauthconsumerkey",
                     consSec =
                         forceEither $ get cp "DEFAULT" "oauthconsumersecret",
                     callback = OOB}
  else case getDefaultKeys cp of
    Just (k, s) -> Just $ Application {consKey = k, consSec = s, 
                                       callback = OOB}
    Nothing -> Nothing
