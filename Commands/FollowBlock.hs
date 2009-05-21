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

module Commands.Follow(follow, unfollow) where
import Utils
import System.Log.Logger
import Data.List
import Download

i = infoM "follow"

follow = simpleCmd "follow" "Start following someone"
             follow_help
             [] follow_worker

follow_worker _ cp ([], [user]) =
    do xmlstr <- sendAuthRequest cp ("/friendships/create/" ++ user ++ ".xml") [] [("id", user)]
       debugM "follow" $ "Got doc: " ++ xmlstr
       -- let doc = getContent . xmlParse "follow" . stripUnicodeBOM $ xmlstr
       -- return ()
       
follow_worker _ _ _ =
    permFail "follow: syntax error; see twidge follow --help"

follow_help =
 "Usage: twidge follow username\n\n\
 \will add username to your list of people you follow.\n\n"


unfollow = simpleCmd "unfollow" "Stop following someone"
             unfollow_help
             [] unfollow_worker

unfollow_worker _ cp ([], [user]) =
    do xmlstr <- sendAuthRequest cp ("/friendships/destroy/" ++ user ++ ".xml") [] [("id", user)]
       debugM "unfollow" $ "Got doc: " ++ xmlstr
       -- let doc = getContent . xmlParse "follow" . stripUnicodeBOM $ xmlstr
       -- return ()

unfollow_worker _ _ _ =
    permFail "unfollow: syntax error; see twidge unfollow --help"

unfollow_help =
 "Usage: twidge unfollow username\n\n\
 \will remove username from the list of people you follow.\n"
