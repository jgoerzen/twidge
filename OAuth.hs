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

twitterKeys = ("t5TWz01unNDrmwngl4fQ",
               "QR2RJVx8R6zdxWybdGDaLlPMqdRrhZDwO7Kn1uoZUc")
identicaKeys = ("f027d666f9d0e7b80beaed528aec473c",
                "d84c9b3dafb14becb5e05a002886b60c")

getDefaultKeys :: ConfigFile -> Maybe (String, String)
getDefaultKeys cp =
  case serverHost cp of
    "twitter.com" -> Just twitterKeys
    "identi.ca" -> Just identicaKeys

getApp :: ConfigFile -> Maybe Application
getApp cp = 
  if (has_option cp "DEFAULT" "oauthconsumerkey" &&
      has_option cp "DEFAULT" "oauthconsumersecret")
  then Just $ Application 
                    {consKey = 
                          fromJust $ get cp "DEFAULT" "oauthconsumerkey",
                       consSecret =
                         fromJust $ get cp "DEFAULT" "oauthconsumersecret"}
  else case getDefaultKeys cp of
    Just (k, s) -> Just $ Application {consKey = k, consSecret = s}
    Nothing -> Nothing
