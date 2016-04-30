{-# LANGUAGE GADTs #-}
module Interpreter
    ( WebFetchT
    , runWebFetchT
    , fetchSeedpage
    , fetchWebpage
    , fetchImage
    , debug
    ) where

import Control.Monad
import Control.Monad.Operational
import Control.Monad.IO.Class
import Pipes

import qualified Data.ByteString.Lazy.UTF8 as UL

import Types


data WebFetchI a where
    FetchSeedpage :: WebFetchI UL.ByteString
    FetchWebpage :: [(Url, Cached)] -> WebFetchI UL.ByteString
    FetchImage :: Url -> ComicTag -> WebFetchI ()
    Debug :: (Show s) => s -> WebFetchI ()

type WebFetchT m a = ProgramT WebFetchI m a

runWebFetchT :: (MonadIO m, Monad m) => WebFetchT (Pipe ReplyType FetchType m) () -> Pipe ReplyType FetchType m ()
runWebFetchT = eval <=< viewT
  where
    eval :: (MonadIO m, Monad m) => ProgramViewT WebFetchI (Pipe ReplyType FetchType m) () -> Pipe ReplyType FetchType m ()
    eval (Return _) = return ()

    -- TODO: need to find a way to make this only run once
    eval (FetchSeedpage :>>= k) =
        await >>= (\(WebpageReply b) -> runWebFetchT (k b))

    eval (FetchWebpage us :>>= k) =
        forM_ us (\(u, c) -> (yield (Webpage u c)) >> await >>= \(WebpageReply b) -> runWebFetchT (k b))

    eval (FetchImage u ct :>>= k) =
        (yield (Image u ct)) >> runWebFetchT (k ())

    eval (Debug s :>>= k) =
        (liftIO $ print s) >> runWebFetchT (k ())

fetchSeedpage :: WebFetchT m UL.ByteString
fetchSeedpage = singleton FetchSeedpage

fetchWebpage :: [(Url, Cached)] -> WebFetchT m UL.ByteString
fetchWebpage = singleton . FetchWebpage

fetchImage :: Url -> ComicTag -> WebFetchT m ()
fetchImage u ct = singleton $ FetchImage u ct

debug :: (Show a) => a -> WebFetchT m ()
debug = singleton . Debug
