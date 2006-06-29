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

{- |
   Module     : FeedParser
   Copyright  : Copyright (C) 2006 John Goerzen
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
import MissingH.Maybe

data Item = Item {itemtitle :: String,
                  enclosureurl :: String,
                  enclosuretype :: String}
          deriving (Eq, Show, Read)

data Feed = Feed {channeltitle :: String,
                  items :: [Item]}
            deriving (Eq, Show, Read)

item2ep pc item =
    Episode {podcast = pc, epid = 0,
             eptitle = sanitize_basic (itemtitle item), 
             epurl = sanitize_basic (enclosureurl item),
             eptype = sanitize_basic (enclosuretype item), epstatus = Pending}

parse :: FilePath -> String -> IO (Either String Feed)
parse fp name = 
    do c <- readFile fp
       case xmlParse' name c of
         Left x -> return (Left x)
         Right y ->
             do let doc = getContent y
                let title = getTitle doc
                let feeditems = getEnclosures doc
                return $ Right $
                           (Feed {channeltitle = title, items = feeditems})
       where getContent (Document _ _ e _) = CElem e

unesc = xmlUnEscape stdXmlEscaper

getTitle doc = strofm "title" (channel doc)

getEnclosures doc =
    concat . map procitem $ item doc
    where procitem i = map (procenclosure title) enclosure
              where title = strofm "title" [i]
                    enclosure = tag "enclosure" `o` children $ i
          procenclosure title e =
              Item {itemtitle = title,
                    enclosureurl = head0 $ forceMaybe $ stratt "url" e,
                    enclosuretype = head0 $ case stratt "type" e of
                                              Nothing -> ["application/octet-stream"]
                                              Just x -> x
                                                }
          head0 [] = ""
          head0 (x:xs) = x
              

item = tag "item" `o` children `o` channel

channel =
    tag "channel" `o` children `o` tag "rss"


--------------------------------------------------
-- Utilities
--------------------------------------------------

attrofelem :: String -> Content -> Maybe AttValue
attrofelem attrname (CElem inelem) =
    case unesc inelem of
      Elem name al _ -> lookup attrname al
attrofelem _ _ =
    error "attrofelem: called on something other than a CElem"
stratt :: String -> Content -> Maybe [String]
stratt attrname content =
    case attrofelem attrname content of
      Just (AttValue x) -> Just (concat . map mapfunc $ x)
      Nothing -> Nothing
    where mapfunc (Left x) = [x]
          mapfunc (Right _) = []

-- Finds the literal children of the named tag, and returns it/them
tagof :: String -> CFilter
tagof x = keep /> tag x -- /> txt

-- Retruns the literal string that tagof would find
strof :: String -> Content -> String
strof x y = verbatim $ tag x /> txt $
            case tagof x $ y of
                [CElem elem] -> CElem (unesc elem)
                z -> error $ "strof: expecting CElem, got " ++ verbatim z

strofm x y = concat . map (strof x) $ y
