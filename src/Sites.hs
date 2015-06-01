{-# LANGUAGE GADTs #-}
import Control.Monad
import Control.Monad.Operational
import Control.Monad.IO.Class


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


-- Reinversion/Operational
data WebFetchI a where
    FetchWebpage :: [String] -> WebFetchI String
    FetchImage :: String -> WebFetchI ()
    Debug :: (Show s) => s -> WebFetchI ()

type WebFetchT m a = ProgramT WebFetchI m a

runWebFetchT :: (MonadIO m, Monad m) => WebFetchT m () -> m ()
runWebFetchT = eval <=< viewT
  where
    eval :: (MonadIO m, Monad m) => ProgramViewT WebFetchI m () -> m ()
    eval (Return _)                 = return ()

    eval (FetchWebpage us :>>= k)   =
        forM_ us (\u -> do
            b <- liftIO $ fetchWebpage u (return . show)
            runWebFetchT (k b))

    eval (FetchImage u :>>= k)      = do
        liftIO $ fetchImage u
        runWebFetchT (k ())

    eval (Debug s :>>= k)           = do
        liftIO $ print s
        runWebFetchT (k ())


fwp :: [String] -> WebFetchT m String
fwp = singleton . FetchWebpage

fi :: String -> WebFetchT m ()
fi = singleton . FetchImage

debug :: (Show a) => a -> WebFetchT m ()
debug = singleton . Debug


s3 :: WebFetchT IO ()
s3 = do
    debug "Approach one"
    liftIO $ putStrLn ""

    idx <- fwp ["idx1"]

    -- Parsing
    debug idx

    chp <- fwp ["url1", "url2", "url3"]

    -- Parse chps
    debug chp

    -- FetchImages
    forM_ ["urla", "urlb", "urlc"] fi

    return ()

s4 :: (Monad m) => WebFetchT m ()
s4 = do
    debug "Approach two"
    debug ""

    -- Another approach
    forM_ [("url1", "urla"), ("url2", "urlb"), ("url3", "urlc")] (\(pageUrl, imgUrl) -> do
        -- Fetch page
        pg <- fwp [pageUrl]

        -- Parsing
        debug pg

        -- Fetch image
        fi imgUrl
        )

    return ()


test = runWebFetchT s3 >> putStrLn "" >> runWebFetchT s4
