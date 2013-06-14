{- hpodder component
Copyright (C) 2006-2013 John Goerzen <jgoerzen@complete.org>

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
   Module     : Download
   Copyright  : Copyright (C) 2006-2013 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

module Download(sendAuthRequest, simpleDownload, twidgeCurlClient) where
import Control.Monad(unless)
import System.Log.Logger
import Data.ConfigFile
import Data.List
import Network.URI
import Data.Either.Utils(forceEither)
import Data.Maybe
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.HttpClient
import Network.Curl
import Network.OAuth.Consumer
import OAuth
import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Lazy.UTF8(toString)

d = debugM "download"
i = infoM "download"

twidgeCurlClient :: CurlClient
twidgeCurlClient = OptionsCurlClient
                   [CurlLowSpeedTime 60
                   ,CurlLowSpeed 1
                   ,CurlUserAgent "twidge v1.1.0; Haskell. GHC"
                   ,CurlFollowLocation True  -- follow redirects
                   ,CurlFailOnError True     -- fail on server errors 
                   ]

{- | Download a webpage without using oauth. -}
simpleDownload :: String -> IO String
simpleDownload url =
  do r <- resp
     d $ "simpleDownload response from URL " ++ show url ++ ": " ++ show r
     return . toString . rspPayload $ r
  where resp = (runClient_ twidgeCurlClient) (fromJust $ parseURL url)

needsUpgrade :: String
needsUpgrade = 
  unlines $ 
  ["Your configuration needs to be updated to work with changes at Twitter."
  ,"Please edit your configuration file and fix the urlbase option.  In most"
  ,"cases, you can set it like this:"
  ,""
  ,"urlbase: %(serverbase)s/1.1"]

sendAuthRequest :: Method -> ConfigParser -> String -> [(String, String)] -> [(String, String)] -> IO ByteString
sendAuthRequest mth cp url getopts postoptlist =
    do app <- case getApp cp of      
         Nothing -> fail $ "Error: auth not set up for this host"
         Just x -> return x
       oauthdata <- case get cp "DEFAULT" "oauthdata" of  
         Left x -> fail $ "Need to (re-)run twidge setup to configure auth: "
                   ++ show x
         Right y -> return y
       
       unless (urlbase /= "https://api.twitter.com/1") $
         fail $ needsUpgrade
       let parsedUrl = fromJust . parseURL $ urlbase ++ url ++ optstr
       
       -- add to the request the GET/POST headers
       let request = parsedUrl {qString = 
                                   fromList (toList (qString parsedUrl) ++
                                                    postoptlist)
                               ,method = mth
                               }
       let token = AccessToken app (fromList (read oauthdata))
       let resp = runOAuthM token $ 
                  do r <- signRq2 HMACSHA1 (Just $ Realm "realm") request
                     serviceRequest twidgeCurlClient r
       r <- resp
       d $ "response: " ++ show r
       return . rspPayload $ r
    where urlbase = forceEither $ get cp "DEFAULT" "urlbase"
          optstr = case getopts of
                     [] -> ""
                     _ -> "?" ++ (concat . intersperse "&" . map conv $ getopts)
          conv (k, v) = k ++ "=" ++ escapeURIString isUnreserved v
