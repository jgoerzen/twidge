{-# LANGUAGE CPP #-}
{-
Copyright (C) 2010-2013 John Goerzen <jgoerzen@complete.org>

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

module Commands.Setup(setup) where
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
import Control.Monad.Trans
import qualified Control.Monad.State.Class as M
import Download(twidgeCurlClient)

i = infoM "setup"
d = debugM "setup"

--------------------------------------------------
-- setup
--------------------------------------------------

setup = simpleCmd "setup" "Interactively configure twidge for first-time use"
        setup_help
        [] setup_worker

setup_worker cpath cp _ =
  do hSetBuffering stdout NoBuffering
     when (has_option cp "DEFAULT" "oauthdata")
       confirmSetup
     putStrLn "\nWelcome to twidge.  We will now configure twidge for your"
     putStrLn "use with Twitter (or a similar service).  This will be quick and easy!\n"
     putStrLn "\nPlease wait a moment while I query the server...\n\n"
       
     app <- case getApp cp of
       Nothing -> fail $ "Error: must specify oauthconsumerkey and oauthconsumersecret for non-default host " ++ (serverHost cp)
       Just x -> return x
     
     let reqUrlBase = forceEither $ get cp "DEFAULT" "oauthrequesttoken"
     let accUrlBase = forceEither $ get cp "DEFAULT" "oauthaccesstoken"
     let authUrlBase = forceEither $ get cp "DEFAULT" "oauthauthorize"
     let reqUrl = fromJust . parseURL $ reqUrlBase
     let accUrl = fromJust . parseURL $ accUrlBase
     let authUrl = ((authUrlBase ++ "?oauth_token=") ++ ) . 
                   findWithDefault ("oauth_token", "") .
                   oauthParams

     let resp = 
           runOAuthM (fromApplication app) $ 
           do liftIO $ d "Trying first signRq2"
              reqres1 <- signRq2 HMACSHA1 Nothing reqUrl
              liftIO $ d $ "First signRq2 result: " ++ (show reqres1)
              oauthRequest twidgeCurlClient reqres1
              
              twidgeAskAuthorization authUrl
              
              liftIO $ d "Trying second signRq2"
              reqres2 <- signRq2 HMACSHA1 Nothing accUrl
              liftIO $ d $ "Second signRq2 result: " ++ show reqres2
              oauthRequest twidgeCurlClient reqres2
              

              tok <- getToken
              return tok
     tok <- resp
     d $ "Got token: " ++ show tok
     case tok of
       AccessToken _ _ -> do
         let newcp = forceEither $ set cp "DEFAULT" "oauthdata" .
                 esc . show . toList . oauthParams $ tok
         writeCP cpath newcp
         putStrLn $ "Successfully authenticated!" 
         putStrLn "Twidge has now been configured for you and is ready to use."
       _ -> putStrLn "Authentication failed; please try again"
    where confirmSetup =
              do putStrLn "\nIt looks like you have already authenticated twidge."
                 putStrLn "If we continue, I may remove your existing"
                 putStrLn "authentication.  Would you like to proceed?"
                 putStr   "\nYES or NO: "
                 c <- getLine
                 if (map toLower c) == "yes"
                    then return ()
                    else permFail "Aborting setup at user request."
          esc x = concatMap fix x
          fix '%' = "%%"
          fix x = [x]


twidgeAskAuthorization :: MonadIO m => (Token -> String) -> OAuthMonadT m ()
twidgeAskAuthorization getUrl = 
  do token <- getToken
     answer <- liftIO $ do putStrLn "OK, next I need you to authorize Twidge to access your account."
                           putStrLn "Please cut and paste this URL and open it in a web browser:\n"
                           putStrLn (getUrl token)
                           putStrLn "\nClick Allow when prompted.  You will be given a numeric"
                           putStrLn "key in your browser window.  Copy and paste it here."
                           putStrLn "(NOTE: some non-Twitter services supply no key; just leave this blank"
                           putStrLn "if you don't get one.)\n"
                           putStr   "Authorization key: "
                           getLine
     putToken (injectOAuthVerifier answer token)

setup_help =
  "Usage: twidge setup\n\n"
