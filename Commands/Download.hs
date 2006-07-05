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

module Commands.Download(cmd, cmd_worker) where
import Utils
import MissingH.Logging.Logger
import DB
import Download
import FeedParser
import Types
import Text.Printf
import Config
import Database.HDBC
import Control.Monad
import Utils
import MissingH.Checksum.MD5
import MissingH.Path.FilePath
import System.IO
import System.Directory
import MissingH.Cmd
import System.Posix.Process
import MissingH.ConfigParser
import MissingH.Str
import MissingH.Either
import Data.List
import System.Exit

d = debugM "download"
i = infoM "download"
w = warningM "download"

cmd = simpleCmd "download" 
      "Downloads all pending podcast episodes (run update first)" helptext 
      [] cmd_worker

cmd_worker gi ([], casts) =
    do podcastlist <- getSelectedPodcasts (gdbh gi) casts
       episodelist <- mapM (getEpisodes (gdbh gi)) podcastlist
       let episodes = filter (\x -> epstatus x == Pending) . concat $ episodelist
       i $ printf "%d episode(s) to consider from %d podcast(s)"
         (length episodes) (length podcastlist)
       mapM_ (downloadEpisode gi) episodes

cmd_worker _ _ =
    fail $ "Invalid arguments to download; please see hpodder download --help"

downloadEpisode gi ep =
    do i $ printf " * Downloading (%.30s) of (%.30s)" 
         (eptitle ep) (castname . podcast $ ep)
       feeddir <- getEnclTmp
       let tmpfp = feeddir ++ "/" ++ md5s (Str (show (ep {epstatus = Pending})))
       r <- getURL (epurl ep) tmpfp
       case r of
         (Success, _) -> procSuccess gi ep tmpfp
         (TempFail, Terminated sigINT) -> 
             do i "Ctrl-C hit; aborting!"
                exitFailure
         (TempFail, _) -> i "    Temporary failure; will retry later"
         _ -> do updateEpisode (gdbh gi) (ep {epstatus = Error})
                 commit (gdbh gi)
                 w "   Error downloading"

procSuccess gi ep tmpfp =
    do cp <- getCP ep idstr fnpart
       let newfn = (strip . forceEither $ (get cp idstr "downloaddir")) ++ "/" ++
                   (strip . forceEither $ (get cp idstr "namingpatt"))
       createDirectoryIfMissing True (fst . splitFileName $ newfn)
       finalfn <- if (eptype ep) == "audio/mpeg" && 
                     not (isSuffixOf ".mp3" newfn)
                  then movefile tmpfp (newfn ++ ".mp3")
                  else movefile tmpfp newfn
       when (isSuffixOf ".mp3" finalfn) $ 
            do d "   Setting ID3 tags..."
               res <- posixRawSystem "id3v2" ["-A", castname . podcast $ ep,
                                 "-t", eptitle ep,
                                 "--WOAS", epurl ep,
--                                 "--WXXX", feedurl . podcast $ ep,
                                 finalfn]
               case res of
                 Exited (ExitSuccess) -> d $ "   id3v2 was successful."
                 Exited (ExitFailure y) -> w $ "   id3v2 returned: " ++ show y
                 Terminated y -> w $ "   id3v2 terminated by signal " ++ show y
                 _ -> fail "Stopped unexpected"
       updateEpisode (gdbh gi) (ep {epstatus = Downloaded})
       commit (gdbh gi)
       
    where idstr = show . castid . podcast $ ep
          fnpart = snd . splitFileName $ epurl ep

getCP :: Episode -> String -> String -> IO ConfigParser
getCP ep idstr fnpart =
    do cp <- loadCP
       return $ forceEither $
              do cp <- if has_section cp idstr
                          then return cp
                          else add_section cp idstr
                 cp <- set cp idstr "safecasttitle" 
                       (sanitize_fn . castname . podcast $ ep)
                 cp <- set cp idstr "epid" (show . epid $ ep)
                 cp <- set cp idstr "castid" idstr
                 cp <- set cp idstr "safefilename" (sanitize_fn fnpart)
                 cp <- set cp idstr "safeeptitle" (sanitize_fn . eptitle $ ep)
                 return cp

movefile old new =
    do realnew <- findNonExisting new
       copyFile old (realnew ++ ".partial")
       renameFile (realnew ++ ".partial") realnew
       removeFile old
       return new

findNonExisting template =
    do dfe <- doesFileExist template
       if (not dfe)
          then return template
          else do let (dirname, fn) = splitFileName template
                  (fp, h) <- openTempFile dirname fn
                  hClose h
                  return fp

helptext = "Usage: hpodder download [castid [castid...]]\n\n" ++ 
           genericIdHelp ++
 "\nThe download command will cause hpodder to download any podcasts\n\
 \episodes that are marked Pending.  Such episodes are usually generated\n\
 \by a prior call to \"hpodder update\".  If you want to combine an update\n\
 \with a download, as is normally the case, you may want \"hpodder fetch\".\n"
