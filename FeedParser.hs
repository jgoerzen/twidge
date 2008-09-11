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

{- |
   Module     : FeedParser
   Copyright  : Copyright (C) 2006-2008 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}
module FeedParser where

import Types
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Utils
import Data.Maybe.Utils
import Data.Char
import Data.Either.Utils
import Data.List
import Data.String.Utils(strip)

{- | Convert [Content] to a printable string, taking care to unescape it.

An implementation without unescaping would simply be:

> contentToString = concatMap (show . content)

Because HaXml's unescaping only works on Elements, we must make sure that
whatever Content we have is wrapped in an Element, then use txt to
pull the insides back out. -}
contentToString :: [Content] -> String
contentToString = 
    concatMap procContent
    where procContent x = 
              verbatim $ keep /> txt $ CElem (unesc (fakeElem x))

          fakeElem :: Content -> Element
          fakeElem x = Elem "fake" [] [x]

          unesc :: Element -> Element
          unesc = xmlUnEscape stdXmlEscaper

stripUnicodeBOM :: String -> String
stripUnicodeBOM ('\xef':'\xbb':'\xbf':x) = x
stripUnicodeBOM x = x

{- | Removes potentially problematic or malicious stuff -}
sanitize :: String -> String
sanitize = strip . map sanitizer
    where sanitizer c 
              | c `elem` "\n\r\0\t" = " "
              | otherwise = c