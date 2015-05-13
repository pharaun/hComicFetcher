{-# LANGUAGE OverloadedStrings #-}
module Sites.Disenchanted
    ( disenchanted
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.Maybe (catMaybes, maybeToList, listToMaybe)

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.List as DL
import qualified Data.List.Split as SL
import qualified Data.Map.Lazy as Map

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

-- xml-html-conduit-lens
import Control.Lens
import Text.Xml.Lens
import Text.HTML.DOM


-- Parser
import Text.Parsec
import Text.Parsec.Text
import Data.Functor.Identity (Identity)
import Control.Applicative ((*>), (<*), (<*>), (<$>), (<$), pure, liftA)
import Data.Monoid (mconcat)


-- Local imports
import Types


--
-- disenchanted - testing xml-conduit
--
disenchanted = Comic
    { comicName = "Disenchanted"
--    , seedPage = "http://www.disenchantedcomic.com/webcomic/1/"
    , seedPage = "http://www.disenchantedcomic.com/webcomic/2/25/"
    , seedType = undefined

    , pageParse = disenchantedPageParse
    }

disenchantedPageParse :: ReplyType t -> IO [FetchType t]
disenchantedPageParse (WebpageReply pg _) = do

    -- Parse episode/page name
    --  #webcomic-logo > h3:nth-child(2)
    let name = pg ^. html
             . deep (attributed (ix "id" . only "webcomic-logo"))
             . node "h3"
             . text
    print name

    -- Parse image
    --  .alignnone
    let image = pg ^. html
              . deep (attributed (ix "id" . only "webcomic-art"))
              . node "p" . node "img"
              . attr "src" . _Just
    print image

    -- Next page
    --  #webcomic-right > a:nth-child(2)
    --  TODO: Its fetching two here, should only fetch one
    let next = pg ^. html
             . deep (attributed (ix "id" . only "webcomic-right"))
             . node "a"
             . attr "href" . _Just
    print next

    -- Next ep
    --  .nav-links > a:nth-child(3)
    --  TODO: fetching invalid url on episode 2
    let nextEp = pg ^. html
               . deep (attributed (ix "class" . only "nav-links"))
               . ix 2
               . attr "href" . _Just
    print nextEp
    putStrLn ""

    print $ parseTitle name image

    return []

--    case parseTitle name image of
--        Left _          -> return []
--        Right (ct, nEp) -> return [ Image (T.unpack image) ct
--                                  , Webpage (T.unpack (if nEp then nextEp else next)) undefined
--                                  ]


parseTitle name url = runParser (titles url) () "" name

titles :: T.Text -> ParsecT T.Text u Identity (ComicTag, Bool)
titles url = do
    wordParse
    space
    ep <- numParse
    space
    char '('
    wordParse
    space
    cur <- numParse
    space
    wordParse
    space
    tot <- numParse
    char ')'
    eof
    return (
        ComicTag "disenchanted" Nothing Nothing (Just $ UnitTag [StandAlone $ Digit ep Nothing Nothing Nothing] Nothing) (Just $ last $ decodePathSegments $ US.fromString $ T.unpack url),
        (cur == tot)
        )

numParse :: ParsecT T.Text u Identity Integer
numParse = liftA read (many1 digit)

wordParse :: ParsecT T.Text u Identity T.Text
wordParse = liftA T.pack (many1 letter)
