{- hpodder component
Copyright (C) 2006 John Goerzen <jgoerzen@complete.org>

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
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

module Download(getURL, Result(..)) where
import MissingH.Cmd
import System.Posix.Process
import Config
import MissingH.Logging.Logger
import Text.Printf
import System.Exit
import System.Directory
import System.Posix.Files

data Result = Success | TempFail | PermFail
            deriving (Eq, Show, Read)

d = debugM "download"
i = infoM "download"

curl = "curl"
curlopts = ["-A", "hpodder v0.1.0; Haskell; GHC", -- Set User-Agent
--            "-s",               -- Silent mode
            "-#",               -- Progress bar
            "-L",               -- Follow redirects
            "-y", "60", "-Y", "1", -- Timeouts
            "--retry", "2",     -- Retry twice
            "-f",               -- Fail on server errors
            "-C", "-"           -- Continue partial downloads
           ]

getCurlConfig :: IO String
getCurlConfig =
    do ad <- getAppDir
       return $ ad ++ "/curlrc"

getURL :: String -> FilePath -> IO (Result, ProcessStatus)
getURL url fp =
    do curlrc <- getCurlConfig
       havecurlrc <- doesFileExist curlrc
       let curlrcopts = if havecurlrc then ["-K", curlrc] else []
       startsize <- getsize
       case startsize of 
         Just x -> d $ printf "Resuming download of %s at %s" fp (show x)
         Nothing -> d $ printf "Beginning download of %s" fp
       ec <- posixRawSystem curl (curlopts ++ curlrcopts ++ [url, "-o", fp])
       newsize <- getsize
       let r = case ec of
                  Exited ExitSuccess -> Success
                  Exited (ExitFailure i) ->
                      case i of
                        5 -> TempFail -- error resolving proxy
                        6 -> TempFail -- error resolving host
                        7 -> TempFail -- couldn't connect
                        9 -> TempFail -- FTP access denied
                        15 -> TempFail -- FTP host problem
                        16 -> TempFail -- FTP connect problem
                        18 -> TempFail -- Partial transfer
                        26 -> TempFail -- Read error
                        27 -> TempFail -- out of memory
                        28 -> TempFail -- timeout
                        35 -> TempFail -- SSL handshake fail
                        37 -> TempFail -- permissions
                        52 -> TempFail -- no reply
                        _ -> PermFail
                  Terminated _ -> TempFail
                  Stopped _ -> TempFail
       if r == Success
          then do d $ "curl returned successfully; new size is " ++
                        (show newsize)
                  if (startsize /= Nothing) && (newsize == startsize)
                     -- compensate for resumes that failed
                     then do i $ "Attempt to resume download failed; re-downloading from start"
                             removeFile fp
                             getURL url fp
                     else if newsize == Nothing
                             -- Sometimes Curl returns success but doesn't 
                             -- actually download anything
                             then return (TempFail, Exited ExitSuccess)
                             else return (r, ec)
          else do d $ "curl returned error; new size is " ++ (show newsize)
                  return (r, ec)

    where getsize = catch (getFileStatus fp >>= (return . Just . fileSize))
                          (\_ -> return Nothing)
