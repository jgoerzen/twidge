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

i = infoM "authenticate"

reqUrl = fromJust . parseURL $ "https://twitter.com/oauth/request_token"
accUrl = fromJust . parseURL $ "https://twitter.com/oauth/access_token"
authUrl = ("https://twitter.com/oauth/authorize?oauth_token=" ++ ) .
          findWithDefault ("oauth_token", "") .
          oauthParams
srvUrl   = fromJust . parseURL $ "http://service/path/to/resource/"
app = Application {consKey = "t5TWz01unNDrmwngl4fQ",
                   consSec = "QR2RJVx8R6zdxWybdGDaLlPMqdRrhZDwO7Kn1uoZUc",
                   callback = OOB}


--------------------------------------------------
-- authenticate
--------------------------------------------------

authenticate = simpleCmd "setup" "Interactively authenticate twidge to server"
               authenticate_help
               [] authenticate_worker

authenticate_worker cpath cp _ =
  do when (has_option cp "DEFAULT" "oauthtoken")
       confirmAuth
     hSetBuffering stdout NoBuffering
     putStrLn "Ready to authenticate twidge to your account."
     let CurlM resp = runOAuth $ do ignite app
                                    oauthRequest HMACSHA1 Nothing reqUrl
                                    cliAskAuthorization authUrl
                                    oauthRequest PLAINTEXT Nothing accUrl
                                    serviceRequest HMACSHA1 Nothing srvUrl
                                    -- getToken
     response <- resp
     print "Done.\n"
    where confirmAuth =
              do putStrLn "\nIt looks like you have already authenticated twidge."
                 putStrLn "If we continue, I may remove your existing"
                 putStrLn "authentication.  Would you like to proceed?"
                 putStr   "\nYES or NO: "
                 c <- getLine
                 if (map toLower c) == "yes"
                    then return ()
                    else permFail "Aborting authentication at user request."
          esc x = concatMap fix x
          fix '%' = "%%"
          fix x = [x]

setup_worker cpath cp _ =
    do when (has_option cp "DEFAULT" "username" || 
             has_option cp "DEFAULT" "password")
            (confirmSetup)
       hSetBuffering stdout NoBuffering
       putStrLn "\nWelcome to twidge.  We will now configure twidge for your"
       putStrLn "use with Twitter.  This will be quick and easy!\n"
       putStrLn "First, what is your username?\n"
       putStr   "Username: "
       username <- getLine
       putStrLn $ "\nWelcome, " ++ username ++ "!  Now I'll need your password.\n"
       putStr   "Password: "
       hSetEcho stdin False
       password <- getLine
       hSetEcho stdin True
       let newcp = forceEither $ set cp "DEFAULT" "username" (esc username)
       let newcp' = forceEither $ set newcp "DEFAULT" "password" (esc password)
       
       writeCP cpath newcp'
       
       putStrLn "\n\ntwidge has now been configured for you.\n"
    where confirmSetup =
              do putStrLn "\nIt looks like you have already configured twidge."
                 putStrLn "If we continue, I may remove your existing"
                 putStrLn "configuration.  Would you like to proceed?"
                 putStr   "\nYES or NO: "
                 c <- getLine
                 if (map toLower c) == "yes"
                    then return ()
                    else permFail "Aborting configuration at user request."
          esc x = concatMap fix x
          fix '%' = "%%"
          fix x = [x]

setup_help =
 "Usage: twidge setup\n\n"

authenticate_help =
  "Usage: twidge authenticate\n\n"
  