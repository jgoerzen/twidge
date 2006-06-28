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

module Commands.Ls(lscasts) where
import Utils
import MissingH.Logging.Logger
import DB
import Types
import Database.HDBC
import Text.Printf
import Control.Monad
import System.Console.GetOpt
import Data.List

i = infoM "ls"

lscasts = simpleCmd "lscasts" "List all configured podcasts on the system" ""
          [Option "l" [] (NoArg ("l", "")) "Long format display -- include URLs in output"] lscasts_worker

lscasts_worker gi (opts, []) =
    do pc <- getPodcasts (gdbh gi)
       printf "%-4s %s\n" "ID" "Title"
       when (islong) (printf "     URL\n")
       mapM_ printpc (sort pc)
    where islong = lookup "l" opts == Just ""
          printpc pc = do printf "%-4d %s\n" (castid pc) (castname pc)
                          when (islong) (printf "     %s\n" (feedurl pc))

