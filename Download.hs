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

module Download(getURL, Result(..), DownloadTok) where
import MissingH.Cmd
import System.Posix.Process
import Config
import MissingH.Logging.Logger
import Text.Printf
import System.Exit
import System.Directory
import System.Posix.Files
import MissingH.Checksum.MD5

data Result = Success | TempFail | PermFail
            deriving (Eq, Show, Read)

type DownloadTok = (ProcessID, String, FilePath)

d = debugM "download"
i = infoM "download"

curl = "curl"
curlopts = ["-A", "hpodder v1.0.0; Haskell; GHC", -- Set User-Agent
            "-s",               -- Silent mode
            "-S",               -- Still show error messages
            "-L",               -- Follow redirects
            "-y", "60", "-Y", "1", -- Timeouts
            "--retry", "2",     -- Retry twice
            "-f",               -- Fail on server errors
           ]

getCurlConfig :: IO String
getCurlConfig =
    do ad <- getAppDir
       return $ ad ++ "/curlrc"

getsize fp = catch (getFileStatus fp >>= (return . Just . fileSize))
             (\_ -> return Nothing)


{- | Begin the download process on the given URL.

Once it has finished, pass the returned token to finishGetURL. -}
startGetURL :: String           -- ^ URL to download
            -> FilePath         -- ^ Directory into which to put downloaded file
            -> Bool             -- ^ Whether to allow resuming
            -> IO DownloadTok   -- ^ Result including path to which the file is being downloaded
startGetURL url dirbase allowresume =
    do curlrc <- getCurlConfig
       havecurlrc <- doesFileExist curlrc
       let curlrcopts = (if havecurlrc then ["-K", curlrc] else [])
                        ++ (if allowresume then ["-C", "-"] else [])
       let fp = dirbase ++ "/" ++ md5s url
       startsize <- getsize fp
       case startsize of 
         Just x -> d $ printf "Resuming download of %s at %s" fp (show x)
         Nothing -> d $ printf "Beginning download of %s" fp
       pid <- forkRawSystem curl (curlopts ++ curlrcopts ++ [url, "-o", fp])
       return (pid, url, fp)

{- | Checks to see how much has been downloaded on the given file.  Also works
after download is complete to get the final size.  Returns Nothing if the
file doesn't exist. -}
checkDownloadSize :: DownloadTok -> IO (Maybe FileOffset)
checkDownloadSize (_, _, fp) = getsize fp

finishGetURL :: DownloadTok -> ProcesSstatus -> IO Result
finishGetURL (_, url, fp) ec =
    do newsize <- getsize fp
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
                     then do i $ "Attempt to resume download failed; will re-try download on next run"
                             removeFile fp
                             --getURL url fp
                             return TempFail
                     else if newsize == Nothing
                             -- Sometimes Curl returns success but doesn't 
                             -- actually download anything
                             then return TempFail
                             else return r
          else do d $ "curl returned error; new size is " ++ (show newsize)
                  return r

