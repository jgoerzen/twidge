module Rss2 where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)


{-Type decls-}

data Rss = Rss Rss_Attrs Channel
         deriving (Eq,Show)
data Rss_Attrs = Rss_Attrs
    { rssVersion :: (Defaultable String)
    } deriving (Eq,Show)
data Channel = ChannelItem ((List1 Item))
             | ChannelTitle_Link_Description_Language_Copyright_ManagingEditor_WebMaster_PubDate_LastBuildDate_Category_Generator_Docs_Cloud_Ttl_Image_TextInput_SkipHours_SkipDays (Title,Link,Description,[(OneOf15 Language Copyright ManagingEditor WebMaster PubDate LastBuildDate Category Generator Docs Cloud Ttl Image TextInput SkipHours SkipDays)])
             deriving (Eq,Show)
data Item = Item (List1 (OneOf2 Title Description)) (Maybe Link)
                 [(OneOf7 Author Category Comments Enclosure Guid PubDate Source)]
          deriving (Eq,Show)
newtype Author = Author String 		deriving (Eq,Show)
data Category = Category Category_Attrs String
              deriving (Eq,Show)
data Category_Attrs = Category_Attrs
    { categoryDomain :: (Maybe String)
    } deriving (Eq,Show)
data Cloud = Cloud Cloud_Attrs String
           deriving (Eq,Show)
data Cloud_Attrs = Cloud_Attrs
    { cloudDomain :: (Maybe String)
    , cloudPort :: (Maybe String)
    , cloudPath :: (Maybe String)
    , cloudRegisterProcedure :: (Maybe String)
    , cloudProtocol :: (Maybe String)
    } deriving (Eq,Show)
newtype Comments = Comments String 		deriving (Eq,Show)
newtype Copyright = Copyright String 		deriving (Eq,Show)
newtype Description = Description String 		deriving (Eq,Show)
newtype Docs = Docs String 		deriving (Eq,Show)
data Enclosure = Enclosure Enclosure_Attrs String
               deriving (Eq,Show)
data Enclosure_Attrs = Enclosure_Attrs
    { enclosureUrl :: String
    , enclosureLength :: String
    , enclosureType :: String
    } deriving (Eq,Show)
newtype Generator = Generator String 		deriving (Eq,Show)
data Guid = Guid Guid_Attrs String
          deriving (Eq,Show)
data Guid_Attrs = Guid_Attrs
    { guidIsPermaLink :: (Defaultable Guid_isPermaLink)
    } deriving (Eq,Show)
data Guid_isPermaLink = Guid_isPermaLink_true  | 
                        Guid_isPermaLink_false
                      deriving (Eq,Show)
newtype Height = Height String 		deriving (Eq,Show)
data Image = Image Url Title Link
                   [(OneOf3 Width Height Description)]
           deriving (Eq,Show)
newtype Language = Language String 		deriving (Eq,Show)
newtype LastBuildDate = LastBuildDate String 		deriving (Eq,Show)
newtype Link = Link String 		deriving (Eq,Show)
newtype ManagingEditor = ManagingEditor String 		deriving (Eq,Show)
newtype Name = Name String 		deriving (Eq,Show)
newtype PubDate = PubDate String 		deriving (Eq,Show)
newtype SkipDays = SkipDays String 		deriving (Eq,Show)
newtype SkipHours = SkipHours String 		deriving (Eq,Show)
data Source = Source Source_Attrs String
            deriving (Eq,Show)
data Source_Attrs = Source_Attrs
    { sourceUrl :: String
    } deriving (Eq,Show)
data TextInput = TextInput Title Description Name Link
               deriving (Eq,Show)
newtype Title = Title String 		deriving (Eq,Show)
newtype Ttl = Ttl String 		deriving (Eq,Show)
newtype Url = Url String 		deriving (Eq,Show)
newtype WebMaster = WebMaster String 		deriving (Eq,Show)
newtype Width = Width String 		deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Rss where
    fromElem (CElem (Elem "rss" as c0):rest) =
        (\(a,ca)->
           (Just (Rss (fromAttrs as) a), rest))
        (definite fromElem "<channel>" "rss" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Rss as a) =
        [CElem (Elem "rss" (toAttrs as) (toElem a))]
instance XmlAttributes Rss_Attrs where
    fromAttrs as =
        Rss_Attrs
          { rssVersion = defaultA fromAttrToStr "2.0" "version" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "version" (rssVersion v)
        ]
instance XmlContent Channel where
    fromElem (CElem (Elem "channel" [] c0):rest) =
        case (fromElem c0) of
        (Just a,_) -> (Just (ChannelItem a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,_) -> (Just (ChannelTitle_Link_Description_Language_Copyright_ManagingEditor_WebMaster_PubDate_LastBuildDate_Category_Generator_Docs_Cloud_Ttl_Image_TextInput_SkipHours_SkipDays a), rest)
                (_,_) ->
                    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (ChannelItem a) = [CElem (Elem "channel" [] (toElem a) )]
    toElem (ChannelTitle_Link_Description_Language_Copyright_ManagingEditor_WebMaster_PubDate_LastBuildDate_Category_Generator_Docs_Cloud_Ttl_Image_TextInput_SkipHours_SkipDays a) = [CElem (Elem "channel" [] (toElem a) )]
instance XmlContent Item where
    fromElem (CElem (Elem "item" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (\(c,cc)->
                 (Just (Item a b c), rest))
              (many fromElem cb))
           (fromElem ca))
        (definite fromElem "(title|description)+" "item" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Item a b c) =
        [CElem (Elem "item" [] (toElem a ++ maybe [] toElem b ++
                                concatMap toElem c))]
instance XmlContent Author where
    fromElem (CElem (Elem "author" [] c0):rest) =
        (\(a,ca)->
           (Just (Author a), rest))
        (definite fromText "text" "author" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Author a) =
        [CElem (Elem "author" [] (toText a))]
instance XmlContent Category where
    fromElem (CElem (Elem "category" as c0):rest) =
        (\(a,ca)->
           (Just (Category (fromAttrs as) a), rest))
        (definite fromText "text" "category" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Category as a) =
        [CElem (Elem "category" (toAttrs as) (toText a))]
instance XmlAttributes Category_Attrs where
    fromAttrs as =
        Category_Attrs
          { categoryDomain = possibleA fromAttrToStr "domain" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "domain" (categoryDomain v)
        ]
instance XmlContent Cloud where
    fromElem (CElem (Elem "cloud" as c0):rest) =
        (\(a,ca)->
           (Just (Cloud (fromAttrs as) a), rest))
        (definite fromText "text" "cloud" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Cloud as a) =
        [CElem (Elem "cloud" (toAttrs as) (toText a))]
instance XmlAttributes Cloud_Attrs where
    fromAttrs as =
        Cloud_Attrs
          { cloudDomain = possibleA fromAttrToStr "domain" as
          , cloudPort = possibleA fromAttrToStr "port" as
          , cloudPath = possibleA fromAttrToStr "path" as
          , cloudRegisterProcedure = possibleA fromAttrToStr "registerProcedure" as
          , cloudProtocol = possibleA fromAttrToStr "protocol" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "domain" (cloudDomain v)
        , maybeToAttr toAttrFrStr "port" (cloudPort v)
        , maybeToAttr toAttrFrStr "path" (cloudPath v)
        , maybeToAttr toAttrFrStr "registerProcedure" (cloudRegisterProcedure v)
        , maybeToAttr toAttrFrStr "protocol" (cloudProtocol v)
        ]
instance XmlContent Comments where
    fromElem (CElem (Elem "comments" [] c0):rest) =
        (\(a,ca)->
           (Just (Comments a), rest))
        (definite fromText "text" "comments" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Comments a) =
        [CElem (Elem "comments" [] (toText a))]
instance XmlContent Copyright where
    fromElem (CElem (Elem "copyright" [] c0):rest) =
        (\(a,ca)->
           (Just (Copyright a), rest))
        (definite fromText "text" "copyright" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Copyright a) =
        [CElem (Elem "copyright" [] (toText a))]
instance XmlContent Description where
    fromElem (CElem (Elem "description" [] c0):rest) =
        (\(a,ca)->
           (Just (Description a), rest))
        (definite fromText "text" "description" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Description a) =
        [CElem (Elem "description" [] (toText a))]
instance XmlContent Docs where
    fromElem (CElem (Elem "docs" [] c0):rest) =
        (\(a,ca)->
           (Just (Docs a), rest))
        (definite fromText "text" "docs" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Docs a) =
        [CElem (Elem "docs" [] (toText a))]
instance XmlContent Enclosure where
    fromElem (CElem (Elem "enclosure" as c0):rest) =
        (\(a,ca)->
           (Just (Enclosure (fromAttrs as) a), rest))
        (definite fromText "text" "enclosure" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Enclosure as a) =
        [CElem (Elem "enclosure" (toAttrs as) (toText a))]
instance XmlAttributes Enclosure_Attrs where
    fromAttrs as =
        Enclosure_Attrs
          { enclosureUrl = definiteA fromAttrToStr "enclosure" "url" as
          , enclosureLength = definiteA fromAttrToStr "enclosure" "length" as
          , enclosureType = definiteA fromAttrToStr "enclosure" "type" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "url" (enclosureUrl v)
        , toAttrFrStr "length" (enclosureLength v)
        , toAttrFrStr "type" (enclosureType v)
        ]
instance XmlContent Generator where
    fromElem (CElem (Elem "generator" [] c0):rest) =
        (\(a,ca)->
           (Just (Generator a), rest))
        (definite fromText "text" "generator" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Generator a) =
        [CElem (Elem "generator" [] (toText a))]
instance XmlContent Guid where
    fromElem (CElem (Elem "guid" as c0):rest) =
        (\(a,ca)->
           (Just (Guid (fromAttrs as) a), rest))
        (definite fromText "text" "guid" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Guid as a) =
        [CElem (Elem "guid" (toAttrs as) (toText a))]
instance XmlAttributes Guid_Attrs where
    fromAttrs as =
        Guid_Attrs
          { guidIsPermaLink = defaultA fromAttrToTyp Guid_isPermaLink_true "isPermaLink" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "isPermaLink" (guidIsPermaLink v)
        ]
instance XmlAttrType Guid_isPermaLink where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "true" = Just Guid_isPermaLink_true
            translate "false" = Just Guid_isPermaLink_false
            translate _ = Nothing
    toAttrFrTyp n Guid_isPermaLink_true = Just (n, str2attr "true")
    toAttrFrTyp n Guid_isPermaLink_false = Just (n, str2attr "false")
instance XmlContent Height where
    fromElem (CElem (Elem "height" [] c0):rest) =
        (\(a,ca)->
           (Just (Height a), rest))
        (definite fromText "text" "height" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Height a) =
        [CElem (Elem "height" [] (toText a))]
instance XmlContent Image where
    fromElem (CElem (Elem "image" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (\(c,cc)->
                 (\(d,cd)->
                    (Just (Image a b c d), rest))
                 (many fromElem cc))
              (definite fromElem "<link>" "image" cb))
           (definite fromElem "<title>" "image" ca))
        (definite fromElem "<url>" "image" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Image a b c d) =
        [CElem (Elem "image" [] (toElem a ++ toElem b ++ toElem c ++
                                 concatMap toElem d))]
instance XmlContent Language where
    fromElem (CElem (Elem "language" [] c0):rest) =
        (\(a,ca)->
           (Just (Language a), rest))
        (definite fromText "text" "language" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Language a) =
        [CElem (Elem "language" [] (toText a))]
instance XmlContent LastBuildDate where
    fromElem (CElem (Elem "lastBuildDate" [] c0):rest) =
        (\(a,ca)->
           (Just (LastBuildDate a), rest))
        (definite fromText "text" "lastBuildDate" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (LastBuildDate a) =
        [CElem (Elem "lastBuildDate" [] (toText a))]
instance XmlContent Link where
    fromElem (CElem (Elem "link" [] c0):rest) =
        (\(a,ca)->
           (Just (Link a), rest))
        (definite fromText "text" "link" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Link a) =
        [CElem (Elem "link" [] (toText a))]
instance XmlContent ManagingEditor where
    fromElem (CElem (Elem "managingEditor" [] c0):rest) =
        (\(a,ca)->
           (Just (ManagingEditor a), rest))
        (definite fromText "text" "managingEditor" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (ManagingEditor a) =
        [CElem (Elem "managingEditor" [] (toText a))]
instance XmlContent Name where
    fromElem (CElem (Elem "name" [] c0):rest) =
        (\(a,ca)->
           (Just (Name a), rest))
        (definite fromText "text" "name" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Name a) =
        [CElem (Elem "name" [] (toText a))]
instance XmlContent PubDate where
    fromElem (CElem (Elem "pubDate" [] c0):rest) =
        (\(a,ca)->
           (Just (PubDate a), rest))
        (definite fromText "text" "pubDate" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (PubDate a) =
        [CElem (Elem "pubDate" [] (toText a))]
instance XmlContent SkipDays where
    fromElem (CElem (Elem "skipDays" [] c0):rest) =
        (\(a,ca)->
           (Just (SkipDays a), rest))
        (definite fromText "text" "skipDays" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (SkipDays a) =
        [CElem (Elem "skipDays" [] (toText a))]
instance XmlContent SkipHours where
    fromElem (CElem (Elem "skipHours" [] c0):rest) =
        (\(a,ca)->
           (Just (SkipHours a), rest))
        (definite fromText "text" "skipHours" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (SkipHours a) =
        [CElem (Elem "skipHours" [] (toText a))]
instance XmlContent Source where
    fromElem (CElem (Elem "source" as c0):rest) =
        (\(a,ca)->
           (Just (Source (fromAttrs as) a), rest))
        (definite fromText "text" "source" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Source as a) =
        [CElem (Elem "source" (toAttrs as) (toText a))]
instance XmlAttributes Source_Attrs where
    fromAttrs as =
        Source_Attrs
          { sourceUrl = definiteA fromAttrToStr "source" "url" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "url" (sourceUrl v)
        ]
instance XmlContent TextInput where
    fromElem (CElem (Elem "textInput" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (\(c,cc)->
                 (\(d,cd)->
                    (Just (TextInput a b c d), rest))
                 (definite fromElem "<link>" "textInput" cc))
              (definite fromElem "<name>" "textInput" cb))
           (definite fromElem "<description>" "textInput" ca))
        (definite fromElem "<title>" "textInput" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (TextInput a b c d) =
        [CElem (Elem "textInput" [] (toElem a ++ toElem b ++ toElem c ++
                                     toElem d))]
instance XmlContent Title where
    fromElem (CElem (Elem "title" [] c0):rest) =
        (\(a,ca)->
           (Just (Title a), rest))
        (definite fromText "text" "title" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Title a) =
        [CElem (Elem "title" [] (toText a))]
instance XmlContent Ttl where
    fromElem (CElem (Elem "ttl" [] c0):rest) =
        (\(a,ca)->
           (Just (Ttl a), rest))
        (definite fromText "text" "ttl" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Ttl a) =
        [CElem (Elem "ttl" [] (toText a))]
instance XmlContent Url where
    fromElem (CElem (Elem "url" [] c0):rest) =
        (\(a,ca)->
           (Just (Url a), rest))
        (definite fromText "text" "url" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Url a) =
        [CElem (Elem "url" [] (toText a))]
instance XmlContent WebMaster where
    fromElem (CElem (Elem "webMaster" [] c0):rest) =
        (\(a,ca)->
           (Just (WebMaster a), rest))
        (definite fromText "text" "webMaster" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (WebMaster a) =
        [CElem (Elem "webMaster" [] (toText a))]
instance XmlContent Width where
    fromElem (CElem (Elem "width" [] c0):rest) =
        (\(a,ca)->
           (Just (Width a), rest))
        (definite fromText "text" "width" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Width a) =
        [CElem (Elem "width" [] (toText a))]


{-Done-}
