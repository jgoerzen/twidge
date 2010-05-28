{-
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

module Commands.Auth(authenticate) where
import Utils
import System.Log.Logger
import Data.List
import Data.ConfigFile
import System.IO
import Data.Either.Utils
import Data.Char
import Config
import Control.Monad(when)
import Network.OAuth.Consumer
import Data.Maybe
import Network.OAuth.Http.Request
import Network.OAuth.Http.HttpClient
import OAuth
import Data.Binary(encode)

i = infoM "authenticate"

srvUrl = fromJust . parseURL $ 
         "http://api.twitter.com/1/statuses/home_timeline.xml"

--------------------------------------------------
-- authenticate
--------------------------------------------------

authenticate = simpleCmd "authenticate" "Interactively authenticate twidge to server"
               authenticate_help
               [] authenticate_worker

authenticate_worker cpath cp _ =
  do hSetBuffering stdout NoBuffering
     when (has_option cp "DEFAULT" "oauthtoken")
       confirmAuth
     putStrLn "Ready to authenticate twidge to your account."
     
     app <- case getApp cp of
       Nothing -> fail $ "Error: must specify oauthconsumerkey and oauthconsumersecret for non-default host " ++ (serverHost cp)
       Just x -> return x
     
     let reqUrlBase = forceEither $ get cp "DEFAULT" "oauthrequesttoken"
     let accUrlBase = forceEither $ get cp "DEFAULT" "oauthaccesstoken"
     let authUrlBase = forceEither $ get cp "DEFAULT" "oauthauthorize"
     let testRequest = fromJust $ parseURL 
                       ((forceEither $ get cp "DEFAULT" "urlbase") ++ "/statuses/user_timeline.xml")
     let reqUrl = fromJust . parseURL $ reqUrlBase
     let accUrl = fromJust . parseURL $ accUrlBase
     let authUrl = ((authUrlBase ++ "?oauth_token=") ++ ) . 
                   findWithDefault ("oauth_token", "") .
          oauthParams
     
     let CurlM resp = runOAuth $ do ignite app
                                    oauthRequest HMACSHA1 Nothing reqUrl
                                    cliAskAuthorization authUrl
                                    oauthRequest HMACSHA1 Nothing accUrl
                                    serviceRequest HMACSHA1 Nothing testRequest
                                    tok <- getToken
                                    return (twoLegged tok, threeLegged tok,
                                            tok)
     (leg2, leg3, response) <- resp
     -- on successful auth, leg3 is True. Otherwise, it is False.
     -- leg1 is always false and r appears to not matter.
     print (leg2, leg3, oauthParams response)
     if leg3 
       then do let newcp = forceEither $ set cp "DEFAULT" "oauthtoken" $
                           findWithDefault ("oauth_token", "INVALID")
                           (oauthParams response)
               let newcp' = forceEither $ set newcp "DEFAULT" "oauthsessionhandle" $
                            findWithDefault ("oauth_session_handle", "INVALID")
                            (oauthParams response)
               writeCP cpath newcp'
               putStrLn "Successfully authenticated; twidge is ready for your use."
       else putStrLn "Authentication failed; please try again"
    where confirmAuth =
              do putStrLn "\nIt looks like you have already authenticated twidge."
                 putStrLn "If we continue, I may remove your existing"
                 putStrLn "authentication.  Would you like to proceed?"
                 putStr   "\nYES or NO: "
                 c <- getLine
                 if (map toLower c) == "yes"
                    then return ()
                    else permFail "Aborting authentication at user request."

authenticate_help =
  "Usage: twidge authenticate\n\n"
  