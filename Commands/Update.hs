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

module Commands.Update(cmd) where
import Utils
import MissingH.Logging.Logger
import DB

i = infoM "update"

cmd = simpleCmd "update" 
      "Re-scan all feeds and update list of needed downloads" helptext 
      [] cmd_worker

cmd_worker _ _ =
    do return ()

helptext = "Running update will cause hpodder to look at each configured podcast.  It\n\
\will download the feed for each one and update its database of available\n\
\episodes.  It will not actually download any episodes; see the download\n\
\command for that."