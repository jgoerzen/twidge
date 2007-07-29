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
   Copyright  : Copyright (C) 2006-2007 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

module Download(startGetURL, finishGetURL, checkDownloadSize, Result(..), 
                DownloadTok(..), getdlfname) where
import System.Cmd.Utils
import System.Posix.Process
import Config
import System.Log.Logger
import Text.Printf
import System.Exit
import System.Directory
import System.Posix.Files
import System.Posix.Process
import System.Posix.Types
import System.Posix.IO
import Data.Hash.MD5
import Control.Exception(evaluate)

data Result = Success | Failure
            deriving (Eq, Show, Read)

data DownloadTok = 
                 DownloadTok {tokpid :: ProcessID,
                              tokurl :: String,
                              tokpath :: FilePath,
                              tokstartsize :: Maybe FileOffset}
                 deriving (Eq, Show, Ord)

d = debugM "download"
i = infoM "download"

curl = "curl"
curlopts = ["-A", "hpodder v1.0.0; Haskell; GHC", -- Set User-Agent
            "-s",               -- Silent mode
            "-S",               -- Still show error messages
            "-L",               -- Follow redirects
            "-y", "60", "-Y", "1", -- Timeouts
            "--retry", "2",     -- Retry twice
            "-f"                -- Fail on server errors
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
       let fp = dirbase ++ "/" ++ getdlfname url
       startsize <- getsize fp
       case startsize of 
         Just x -> d $ printf "Resuming download of %s at %s" fp (show x)
         Nothing -> d $ printf "Beginning download of %s" fp
       
       msgfd <- openFd (fp ++ ".msg") WriteOnly (Just 0o600)
                (defaultFileFlags {trunc = True})
       msgfd2 <- dup msgfd
       pid <- pOpen3Raw Nothing (Just msgfd) (Just msgfd2) 
                 curl (curlopts ++ curlrcopts ++ [url, "-o", fp])
                 (return ())
       closeFd msgfd
       closeFd msgfd2
       return $ DownloadTok pid url fp startsize

getdlfname url = md5s (Str url)
{- | Checks to see how much has been downloaded on the given file.  Also works
after download is complete to get the final size.  Returns Nothing if the
file doesn't exist. -}
checkDownloadSize :: DownloadTok -> IO (Maybe FileOffset)
checkDownloadSize dltok = getsize (tokpath dltok)

finishGetURL :: DownloadTok -> ProcessStatus -> IO Result
finishGetURL dltok ec =
    do newsize <- getsize (tokpath dltok)
       let r = case ec of
                  Exited ExitSuccess -> Success
                  Exited (ExitFailure i) -> Failure
                  Terminated _ -> Failure
                  Stopped _ -> Failure
       if r == Success
          then do d $ "curl returned successfully; new size is " ++
                        (show newsize)
                  if (tokstartsize dltok /= Nothing) && 
                         (newsize == tokstartsize dltok)
                     -- compensate for resumes that failed
                     then do i $ "Attempt to resume download failed; will re-try download on next run"
                             removeFile (tokpath dltok)
                             --getURL url fp
                             return Failure
                     else if newsize == Nothing
                             -- Sometimes Curl returns success but doesn't 
                             -- actually download anything
                             then return Failure
                             else return r
          else do d $ "curl returned error; new size is " ++ (show newsize)
                  return r

