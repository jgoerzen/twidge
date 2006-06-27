{- hspod component
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
   Module     : Types
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}
module Types where

data EpisodeStatus = Pending -- ^ Ready to download
                   | Downloaded -- ^ Already downloaded
                   | Error -- ^ Error downloading
                   | Skipped -- ^ Skipped by some process or other
                     deriving (Eq, Show, Read, Ord, Enum)

data Podcast = Podcast {castid :: Integer,
                        castname :: String
                        feedurl :: String}
             deriving (Eq, Show, Read)

data Episode = Episode {podcast :: Podcast,
                        eptitle :: String,
                        epurl :: String,
                        epstatus :: EpisodeStatus}
             deriving (Eq, Show, Read)
