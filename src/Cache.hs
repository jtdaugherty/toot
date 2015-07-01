{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}
module Cache
  ( readTweetCache
  , writeTweetCache
  )
where

import Control.Applicative
import Control.Exception (SomeException, catch)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL

import Web.Twitter.Types.Lens (Status)

tweetCacheFile :: FilePath
tweetCacheFile = "tweetcache.txt"

readTweetCache :: IO (Maybe [Status])
readTweetCache =
    A.decode <$> (BSL.readFile tweetCacheFile `catch` \(_::SomeException) -> return "[]")

tweetsToCache :: Int
tweetsToCache = 100

writeTweetCache :: [Status] -> IO ()
writeTweetCache =
    BSL.writeFile tweetCacheFile
    . A.encode
    . reverse
    . take tweetsToCache
    . reverse
