{-# LANGUAGE Rank2Types, DeriveDataTypeable #-}
module Types
    ( Url
    , FetchType(..)
    , ReplyType(..)

    , ComicTag(..)
    , UnitTag(..)

    -- Parsing types
    , Digits(..)
    , Digit(..)
    , SubDigit(..)

    , Comic(..)

    , DebugException(..)
    ) where

import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.Text as T

import Control.Exception
import Data.Typeable

-- Filesystem format - SiteName/StoryName/Volume/Chapter/Page.*
--
-- Other stuff - webcomic
-- 1. Sequal scan (no vol/chp/etc) - Px
-- 2. Chp - CxPx
-- 3. Vol - VxPx
-- 4. (Story/Track) Vol Chp - SxVxCxPx
-- 5. Vol Act Intermission - VxPx - Act/Intermission can act as a title
--
-- 6. Manga - CxPx, VxCxPx, V(TBD)xCxPx etc... (Probably has story track
--      too for multi story mangas)
--
-- 7. There can be named volumes, named chapters. Don't think i've seen
--      named pages (special pages such as cover, etc)
--
--
-- There is probably some common logic that can be employed for this stuff
-- ^ but majority of it is going to be figuring out how to extract useful
-- semantics, There seems to be several format/manner of indexing/paging.
--
-- SEQUAL
-- 1. Page
-- 2. Chapter/Page
-- 3. Volume/Page
-- 4. Volume/Chapter/Page
--
-- INDEX based
-- 1. Page
-- 2. Chapter/Page
-- 3. Volume/Page
-- 4. Volume/Chapter/Page
--
-- MIXED (Common-ness)
-- 1. Index Chapter/Volume, Sequal Page

---- Filesystem format - SiteName/StoryName/Volume/Chapter/Page.*
data ComicTag = ComicTag
    { ctSiteName :: T.Text
    , ctStoryName :: Maybe T.Text

    , ctVolume :: Maybe UnitTag
    , ctChapter :: Maybe UnitTag

    , ctFileName :: Maybe T.Text -- TODO: need to find a way to make this mandatory...
--    , ctPage :: Maybe UnitTag --  TODO: Implement this, for now we just use file name
    }
    deriving (Show)

data UnitTag = UnitTag
    { utNumber :: [Digits]
    , utTitle :: Maybe T.Text
    }
    deriving (Show)



data Digits = RangeDigit Digit Digit
            | StandAlone Digit
            deriving (Show, Eq)

-- Int: 29 subdigit: .9 Int: v1 Text: a -> 29.9v1a
data Digit = Digit Integer (Maybe SubDigit) (Maybe Integer) (Maybe T.Text)
           deriving (Show, Eq)

-- Sub Digits
data SubDigit = DotSubDigit (Maybe Integer) T.Text
              deriving (Show, Eq)






-- Data type of the url and any additional info needed
type Url = String

data ReplyType a = WebpageReply UL.ByteString a

data FetchType a = Webpage Url a
                 | Image Url ComicTag -- TODO: this is probably wrong type - We probably want FPO.FilePath



-- Seems like we can just hit the main page to find a chapter/volume to go
-- to then parse it out of the dropdown.
--
-- Format:
-- Vol 12 Ch 079: Name V
-- Vol TBD Ch 353: Foobar stuff
-- Ch 087: weird
-- Ch 000
-- Ch 032.5
-- Ch 032.1
-- Ch 087v2: foobar
-- Ch.23: foo
-- Vol.43 Ch.3234: Barz
-- Vol.32 Ch.123 Read Online
-- Vol.4 Chp.Extra: Foobar
-- Vol.4 Chp.Extra 2: Foobar
-- Ch.0: [Oneshot]
-- Ch.0: [Complete]
-- Vol.1 Ch.3 Extra: bar
-- Vol.03 Ch.13a
-- Vol.03 Ch.13cd
-- Vol.4 Chp.23-27: Foobar
-- Vol.2 Chp.Fanbook Omake Read Online
--
-- Need to special case the case in which its Licensed and thus not
-- available for downloading
--
-- Most other sites seems to be mainly:
-- Chp 342
--
-- Flow
--  - Get index page (to make pick up new manga easy)
--  - Make sure its not already licensed (re index page or vol/chp page)
--  - Pick first page/chp/vol and load that to get drop down list, submit
--      one chp fetcher per line in that list

-- Parameterized type
data Comic t = Comic
    { comicName :: String

    -- Seed page/type for kickstarting the parser/fetcher
    , seedPage :: String
--    , seedComicTag :: ComicTag
    , seedType :: t -- TAG

    -- Page parser, Parse a page and return a list of stuff to fetch,
    , pageParse :: ReplyType t -> IO [FetchType t]

    -- TODO: need some good way of convoying/tracking state for things such
    -- as page numbering (page1, page2, page3...)
    }






-- Debugging exceptions - Where, what
data DebugException = DebugException String String
    deriving (Show, Typeable)

instance Exception DebugException
