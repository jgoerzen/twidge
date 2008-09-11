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

module Download(sendAuthRequest) where
import System.Cmd.Utils
import System.Posix.Process
import Config
import System.Log.Logger
import Text.Printf
import Data.ConfigFile
import HSH
import Data.Either.Utils(forceEither)
import Network.URI
import Data.List

d = debugM "download"
i = infoM "download"

curl = "curl"
curlopts = ["-A", "twidge v1.0.0; Haskell; GHC", -- Set User-Agent
            "-s",               -- Silent mode
            "-S",               -- Still show error messages
            "-L",               -- Follow redirects
            "-y", "60", "-Y", "1", -- Timeouts
            "--retry", "2",     -- Retry twice
            "-f"                -- Fail on server errors
           ]

sendAuthRequest :: ConfigParser -> String -> [(String, String)] -> IO String
sendAuthRequest cp url opts =
    do let authopts = getAuthOpts cp
       let urlbase = forceEither $ get cp "DEFAULT" "urlbase"
       run $ (curl, curlopts ++ authopts ++ [urlbase ++ url])
    where optstr = case opts of
                     [] -> ""
                     _ -> "?" ++ (concat . intersperse "&" . map conv $ opts)
          conv (k, v) = k ++ "=" ++ escapeURIString isUnreserved v

getAuthOpts :: ConfigParser -> [String]
getAuthOpts cp =
    case (get cp "DEFAULT" "username", get cp "DEFAULT" "password") of
      (Right user, Right pass) ->
          ["--user", user ++ ":" ++ pass]
      _ -> error "Missing username or password section in config file"
