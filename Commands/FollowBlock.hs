{-
Copyright (C) 2006-2009 John Goerzen <jgoerzen@complete.org>

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

module Commands.FollowBlock(follow, unfollow, block, unblock) where
import Utils
import System.Log.Logger
import Download
import Network.OAuth.Http.Request

i = infoM "followblock"

follow = simpleCmd "follow" "Start following someone"
             follow_help
             [] follow_worker
follow_worker = generic_worker POST "/friendships/create.json" "follow"
follow_help = generic_add_help "follow"

unfollow = simpleCmd "unfollow" "Stop following someone"
             unfollow_help
             [] unfollow_worker
unfollow_worker = generic_worker POST "/friendships/destroy.json" "unfollow"
unfollow_help = generic_rm_help "follow"

block = simpleCmd "block" "Start blocking someone"
        block_help [] block_worker
block_worker = generic_worker POST "/blocks/create.json" "block"
block_help = generic_add_help "block"

unblock = simpleCmd "unblock" "Stop blocking someone"
          unblock_help [] unblock_worker
unblock_worker = generic_worker POST "/blocks/destroy.json" "unblock"
unblock_help = generic_rm_help "block"

generic_worker method url cmdname _ cp ([], [user_string]) =
    do let user = strip_at user_string
       json <- sendAuthRequest method cp url [] [("screen_name", user)]
       debugM cmdname $ "Got doc: " ++ show json
       -- return ()

       where strip_at ('@':u) = u
             strip_at u = u

generic_worker _ _ cmdname _ _ _ =
    permFail $ "follow: syntax error; see twidge " ++ cmdname ++ " --help"

generic_add_help cmd = 
 "Usage: twidge " ++ cmd ++ " username\n\n\
 \will add username to your list of people you " ++ cmd ++ ".\n\n"

generic_rm_help cmd =
 "Usage: twidge un" ++ cmd ++ " username\n\n\
 \will remove username from the list of people you " ++ cmd ++ ".\n"
