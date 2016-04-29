{-# LANGUAGE OverloadedStrings #-}
module Sites.Disenchanted
    ( disenchanted
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as US

-- xml-html-conduit-lens
import Control.Lens
import Text.Xml.Lens hiding (name)

-- Parser
import Text.Parsec
import Control.Applicative (liftA)

-- Local imports
import Types
import Sites.Util (toPipeline)


--
-- disenchanted - testing xml-conduit
--
disenchanted = Comic
    { comicName = "Disenchanted"
    , seedPage = "http://www.disenchantedcomic.com/webcomic/1"
    , seedType = undefined
    , pageParse = toPipeline disenchantedPageParse
    , cookies = []
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
    --  ["http://www.disenchantedcomic.com/webcomic/1/2/"]
    --  ["http://www.disenchantedcomic.com/webcomic/1/2/","http://www.disenchantedcomic.com/webcomic/1/4/"]
    let next = head $ reverse (pg ^.. html
             . deep (attributed (ix "id" . only "webcomic-right"))
             . node "a"
             . attr "href" . _Just)
    print next

    -- Next ep
    --  .nav-links > a:nth-child(3)
    --  ["/webcomic/1/","/webcomic/","http://www.disenchantedcomic.com/webcomic/2/","http://freakangels.com/whitechapel/","/city-maps/"]
    --  ["http://www.disenchantedcomic.com/webcomic/1/","/webcomic/1/","/webcomic/","http://www.disenchantedcomic.com/webcomic/3/","http://freakangels.com/whitechapel/","/city-maps/"]
    let nextEp = head $ tail $ tail $ reverse (pg ^.. html
               . deep (attributed (ix "class" . only "nav-links"))
               . node "a"
               . attr "href" . _Just)
    print nextEp
    putStrLn ""

    case parseTitle name image of
        Left _          -> return []
        Right (ct, nEp) -> return [ Image (T.unpack image) ct
                                  , Webpage (T.unpack (if nEp then nextEp else next)) undefined
                                  ]


parseTitle name url = runParser (titles url) () "" name

titles :: T.Text -> ParsecT T.Text u Identity (ComicTag, Bool)
titles url = do
    _ <- wordParse
    _ <- space
    ep <- numParse
    _ <- space
    _ <- char '('
    _ <- wordParse
    _ <- space
    cur <- numParse
    _ <- space
    _ <- wordParse
    _ <- space
    tot <- numParse
    _ <- char ')'
    eof
    return (
        ComicTag "disenchanted" Nothing Nothing (Just $ UnitTag [StandAlone $ Digit ep Nothing Nothing Nothing] Nothing) (Just $ T.concat
            [ T.justifyRight 2 '0' $ T.pack $ show cur
            , "-"
            , last $ decodePathSegments $ US.fromString $ T.unpack url
            ]),
        (cur == tot)
        )

numParse :: ParsecT T.Text u Identity Integer
numParse = liftA read (many1 digit)

wordParse :: ParsecT T.Text u Identity T.Text
wordParse = liftA T.pack (many1 letter)
