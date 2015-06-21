module Sites.Util
    ( fixChapter
    , fixVolume
    , filterAny
    , toPipeline

    , module Parser.VolChpParser
    , module Parser.Words
    ) where

import Types
import Parser.VolChpParser
import Parser.Words

import qualified Control.Monad as CM
import Pipes

fixChapter :: String -> Maybe UnitTag
fixChapter = ctChapter . volChpParse "" Nothing

fixVolume :: String -> Maybe UnitTag
fixVolume = ctVolume . volChpParse "" Nothing

filterAny :: [a -> Bool] -> [a] -> [a]
filterAny [] [] = []
filterAny [] xs = xs
filterAny xs ys = filter (or . flip map xs . flip id) ys

toPipeline :: (ReplyType t -> IO [FetchType t]) -> Pipe (ReplyType t) (FetchType t) IO ()
toPipeline old = CM.forever $ do
    a <- await
    b <- liftIO $ old a
    mapM_ yield b
