module Main where

import Control.Lens ((&), (^.), (.~))
import Control.Concurrent (forkIO, newChan)
import Web.Twitter.Types.Lens (statusId)
import Data.Default (def)

import Graphics.Vty (mkVty)
import Brick.Main

import Types
import App (app, initialAppState, refreshInterval)
import TimelineThread
import AuthConfig
import Cache

main :: IO ()
main = do
    twInfo <- readAuthConfig

    -- Attempt to read tweet cache
    mCacheData <- readTweetCache
    let cacheData = maybe [] id mCacheData
        lastStatus = if null cacheData
                     then Nothing
                     else Just $ (last cacheData)^.statusId

    timelineUpdateChan <- newChan
    forkIO $ timelineUpdateThread twInfo refreshInterval timelineUpdateChan lastStatus
    finalState <- customMain (mkVty def) timelineUpdateChan app $ initialAppState & timeline .~ cacheData

    writeTweetCache $ finalState^.timeline
