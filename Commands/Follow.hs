{-
Copyright (C) 2006-2008 John Goerzen <jgoerzen@complete.org>

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

module Commands.Follow(follow) where
import Utils
import System.Log.Logger
import Types
import Text.Printf
import System.Console.GetOpt
import Data.List
import Text.XML.HaXml
import Download
import FeedParser

i = infoM "follow"

--------------------------------------------------
-- lscasts
--------------------------------------------------

follow = simpleCmd "follow" "Start following someone"
             follow_help
             [] follow_worker

follow_worker _ cp ([], [user]) =
    do xmlstr <- sendAuthRequest cp ("/friendships/create/" ++ user ++ ".xml") [] [("id", user)]
       debugM "follow" $ "Got doc: " ++ xmlstr
       let doc = getContent . xmlParse "follow" . stripUnicodeBOM $ xmlstr
       return ()
       
    where getContent (Document _ _ e _) = CElem e

          getStatuses = tag "statuses" /> tag "status"
          procStatuses :: Content -> (String, String)
          procStatuses item = 
              (sanitize $ contentToString 
                  (keep /> tag "user" /> tag "screen_name" /> txt $ item),
               sanitize $ contentToString
                  (keep /> tag "text" /> txt $ item)
              )


follow_help =
 "Usage: twidge follow username\n\n\
 \will add username to your list of people you follow.\n\n"
