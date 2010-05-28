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
   Module     : Download
   Copyright  : Copyright (C) 2006-2008 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

module Download(sendAuthRequest, simpleDownload) where
import System.Log.Logger
import Data.ConfigFile
import Data.List
import Network.URI
import Data.Either.Utils(forceEither)
import Data.Maybe
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Consumer
import TwidgeHttpClient
import OAuth
import Data.ByteString.Lazy.UTF8(toString)

d = debugM "download"
i = infoM "download"

{-
curl = "curl"
curlopts = ["-A", "twidge v1.0.0; Haskell; GHC", -- Set User-Agent
            "-s",               -- Silent mode
            "-S",               -- Still show error messages
            "-L",               -- Follow redirects
            "-y", "60", "-Y", "1", -- Timeouts
            "--retry", "2",     -- Retry twice
            "-f"                -- Fail on server errors
           ]
-}

simpleDownload :: String -> IO String
{-
simpleDownload url = run (curl, curlopts ++ [url])
-}
simpleDownload _ = fail "simpleDownload not yet implemented"

sendAuthRequest :: ConfigParser -> String -> [(String, String)] -> [(String, String)] -> IO String
sendAuthRequest cp url getopts postoptlist =
    do app <- case getApp cp of      
         Nothing -> fail $ "Error: auth not set up"
         Just x -> return x
       oauthdata <- case get cp "DEFAULT" "oauthdata" of  
         Left x -> fail $ "Need to (re-)run twidge setup to configure auth: "
                   ++ show x
         Right y -> return y
       
       let parsedUrl = fromJust . parseURL $ urlbase ++ url ++ optstr
       
       -- add to the request the POST headers
       let request = parsedUrl {reqHeaders = 
                                   fromList (toList (reqHeaders parsedUrl) ++
                                                    postoptlist)
                               }
       
       let CurlM resp = runOAuth $ 
                        do ignite app
                           putToken $ AccessToken 
                                       {application = app,
                                        oauthParams = fromList (read oauthdata)
                                       }
                           serviceRequest HMACSHA1 Nothing request
       r <- resp
       d $ "response: " ++ show r
       return . toString . rspPayload $ r
    where urlbase = forceEither $ get cp "DEFAULT" "urlbase"
          optstr = case getopts of
                     [] -> ""
                     _ -> "?" ++ (concat . intersperse "&" . map conv $ getopts)
          conv (k, v) = k ++ "=" ++ escapeURIString isUnreserved v

getAuthOpts :: ConfigParser -> [String]
getAuthOpts cp =
    case (get cp "DEFAULT" "username", get cp "DEFAULT" "password") of
      (Right user, Right pass) ->
          ["--user", user ++ ":" ++ pass]
      _ -> error "Missing username or password option in config file"
