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

module Commands.Add(cmd) where
import Utils
import MissingH.Logging.Logger
import DB
import Types
import Database.HDBC
import Text.Printf

i = infoM "add"

cmd = simpleCmd "add" 
      "Add a new podcast" helptext 
      [] cmd_worker

cmd_worker gi (_, [url]) =
    do pc <- addPodcast (gdbh gi) (Podcast {castid = 0,
                                            castname = "NEWLY ADDED PODCAST",
                                            feedurl = url})
       commit (gdbh gi)
       printf "Podcast added:\n    URL: %s\n    ID: %d\n" url (castid pc)

cmd_worker _ _ =
    do fail "Feed URL required; see hpodder add --help for info"

helptext = 
    "Usage: hpodder add feedurl"
