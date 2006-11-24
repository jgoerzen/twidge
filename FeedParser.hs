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
import Data.Char
import MissingH.Either
import Data.List

data Item = Item {itemtitle :: String,
                  enclosureurl :: String,
                  enclosuretype :: String,
                  enclosurelength :: String
                  }
          deriving (Eq, Show, Read)

data Feed = Feed {channeltitle :: String,
                  items :: [Item]}
            deriving (Eq, Show, Read)

item2ep pc item =
    Episode {podcast = pc, epid = 0,
             eptitle = sanitize_basic (itemtitle item), 
             epurl = sanitize_basic (enclosureurl item),
             eptype = sanitize_basic (enclosuretype item), epstatus = Pending,
             eplength = case reads . sanitize_basic . enclosurelength $ item of
                          [] -> 0,
                          [(x, [])] -> x
                          _ -> 0}

parse :: FilePath -> String -> IO (Either String Feed)
parse fp name = 
    do c <- readFile fp
       case xmlParse' name (unifrob c) of
         Left x -> return (Left x)
         Right y ->
             do let doc = getContent y
                let title = getTitle doc
                let feeditems = getEnclosures doc
                return $ Right $
                           (Feed {channeltitle = title, items = feeditems})
       where getContent (Document _ _ e _) = CElem e

unifrob ('\xef':'\xbb':'\xbf':x) = x -- Strip off unicode BOM
unifrob x = x

unesc = xmlUnEscape stdXmlEscaper

getTitle doc = forceEither $ strofm "title" (channel doc)

getEnclosures doc =
    concat . map procitem $ item doc
    where procitem i = map (procenclosure title) enclosure
              where title = case strofm "title" [i] of
                              Left x -> "Untitled"
                              Right x -> x
                    enclosure = tag "enclosure" `o` children $ i
          procenclosure title e =
              Item {itemtitle = title,
                    enclosureurl = head0 $ forceMaybe $ stratt "url" e,
                    enclosuretype = head0 $ case stratt "type" e of
                                              Nothing -> ["application/octet-stream"]
                                              Just x -> x,
                    enclosurelength = head $ case stratt "length" e of
                                                Nothing -> ["0"]
                                                Just [] -> ["0"]
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
strof x y = forceEither $ strof_either x y

strof_either :: String -> Content -> Either String String
strof_either x y =
    case tagof x $ y of
      [CElem elem] -> Right $ verbatim $ tag x /> txt $ CElem (unesc elem)
      z -> Left $ "strof: expecting CElem in " ++ x ++ ", got "
           ++ verbatim z ++ " at " ++ verbatim y

strofm x y = 
    if length errors /= 0
       then Left errors
       else Right (concat plainlist)
    where mapped = map (strof_either x) $ y
          (errors, plainlist) = conveithers mapped
          isright (Left _) = False
          isright (Right _) = True

conveithers :: [Either a b] -> ([a], [b])
conveithers inp =  worker inp ([], [])
    where worker [] y = y
          worker (Left x:xs) (lefts, rights) =
              worker xs (x:lefts, rights)
          worker (Right x:xs) (lefts, rights) =
              worker xs (lefts, x:rights)

