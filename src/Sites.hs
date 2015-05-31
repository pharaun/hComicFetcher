{-# LANGUAGE GADTs #-}
import Control.Monad
import Control.Monad.Operational
import Control.Monad.IO.Class

-- Tags
data CTag = Initial -- The initial page
          | Page Integer
          deriving (Show)

data ReplyType a = WebpageReply String a

instance (Show a) => Show (ReplyType a) where
    show (WebpageReply c t) = "WebpageReply: " ++ c ++ " - " ++ show t

data FetchType a = Webpage String a
                 | Image String

instance (Show a) => Show (FetchType a) where
    show (Webpage c t) = "Webpage: " ++ c ++ " - " ++ show t
    show (Image c) = "Image: " ++ c


--
-- Callbacky solution with wrapping up the needed metadata as tags
--
callback :: ReplyType CTag -> IO [FetchType CTag]
callback (WebpageReply _ Initial) = do
    return [Webpage "url1" (Page 1), Webpage "url2" (Page 2), Webpage "url3" (Page 3)]

callback (WebpageReply _ (Page pg)) =
    case pg of
        1   -> return [Image "img1"]
        2   -> return [Image "img2"]
        3   -> return [Image "img3"]
        _   -> return [Image "img_"]



data ReplyType' = WebpageReply' String
    deriving (Show, Read)


fetchWebpage url action = action b
  where
    b = case url of
        "idx1"  -> WebpageReply' "index1"
        "url1"  -> WebpageReply' "content1"
        "url2"  -> WebpageReply' "content2"
        "url3"  -> WebpageReply' "content3"
        _       -> WebpageReply' "Unknown"

fetchImage x = print $ case x of
                    "urla"  -> "contenta"
                    "urlb"  -> "contentb"
                    "urlc"  -> "contentc"
                    _       -> "Unknown"

-- DFS
s1 = do
        _ <- fetchWebpage "idx1" (\idx -> do

            -- Parsing
            print idx

            forM ["url1", "url2", "url3"] $ flip fetchWebpage (\chp -> do

                -- do parsing stuff
                print chp

                forM ["urla", "urlb", "urlc"] fetchImage
                )
            )
        return ()

-- BFS
s2 = do
    chpUrls <- fetchWebpage "idx1" (\idx -> do
            -- Parsing
            print idx
            return ["url1", "url2", "url3"])

    imgUrls <- forM chpUrls $ flip fetchWebpage (\chp -> do
            -- Parsing
            print chp
            return ["urla", "urlb", "urlc"])

    forM_ (concat imgUrls) fetchImage

    return ()


-- Reinversion/Operational
data WebFetch a where
    FetchWebpage :: String -> WebFetch String
    FetchImage :: String -> WebFetch ()
    Debug :: (Show s) => s -> WebFetch ()

type WebFetchMonadT m a = ProgramT WebFetch m a

runWebFetchMonadT :: (Monad m, MonadIO m) => WebFetchMonadT m a -> m a
runWebFetchMonadT = eval <=< viewT
  where
    eval :: (Monad m, MonadIO m) => ProgramViewT WebFetch m a -> m a
    eval (Return x)                 = return x
    eval (FetchWebpage u :>>= k)    = do
        b <- liftIO $ fetchWebpage u (return . show)
        runWebFetchMonadT (k b)

    eval (FetchImage u :>>= k)      = do
        liftIO $ fetchImage u
        runWebFetchMonadT (k ())

    eval (Debug s :>>= k)           = do
        liftIO $ print s
        runWebFetchMonadT (k ())


fwp :: (Monad m) => String -> WebFetchMonadT m String
fwp = singleton . FetchWebpage

fi :: (Monad m) => String -> WebFetchMonadT m ()
fi = singleton . FetchImage

debug :: (Show a, Monad m) => a -> WebFetchMonadT m ()
debug = singleton . Debug


s3 :: Monad m => WebFetchMonadT m ()
s3 = do
    idx <- fwp "idx1"

    -- Parsing
    debug idx

    chp <- forM ["url1", "url2", "url3"] fwp

    -- Parse chps
    debug chp

    -- FetchImages
    forM_ ["urla", "urlb", "urlc"] fi

    return ()
