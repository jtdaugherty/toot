{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}
module TimelineThread
  ( timelineUpdateThread
  )
where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Exception (catch)
import Control.Concurrent (Chan, threadDelay, writeChan)
import Data.Monoid
import qualified Data.Text as T
import Web.Twitter.Conduit.Monad
import Web.Twitter.Types.Lens

import Types
import Spinner
import Twitter

timelineUpdateThread :: TWInfo -> Integer -> Chan TootEvent -> Maybe StatusId -> IO ()
timelineUpdateThread twInfo refreshInterval chan since = do
  let doFetch = do
          result <- withSpinner chan $ do
            writeChan chan $ Notify "notification" "Fetching..."
            getHomeTimeline twInfo since

          case result of
            Right ss -> do
                writeChan chan $ NewTweets ss
                writeChan chan ClearNotify
                return $ if null ss then Nothing else Just $ (last ss)^.statusId
            Left e -> do
                writeChan chan $ Notify "error" $ T.pack $ "Error fetching tweets: " ++ e
                return since

  next <- doFetch `catch` \(e::TwitterError) -> do
      case e of
          FromJSONError _ -> writeChan chan $ Notify "error" "JSON error"
          TwitterStatusError _ _ _ -> writeChan chan $ Notify "error" "Status error"
          TwitterUnknownErrorResponse _ _ _ -> writeChan chan $ Notify "error" "Unknown twitter error"
          TwitterErrorResponse _ _ ms -> do
              writeChan chan $ Notify "error" (T.concat $ twitterErrorMessage <$> ms)
              threadDelay 10000000
      return since

  forM_ (enumFromThenTo refreshInterval (refreshInterval - 1) 1) $ \(s::Integer) -> do
      let mins = s `div` 60
          secs = s `mod` 60
          minStr = if mins == 0 then "" else show mins <> "m"
          secStr = show secs <> "s"
          timeStr = minStr <> secStr
      writeChan chan $ Notify "notification" $ T.concat ["Next refresh: ", T.pack timeStr]
      threadDelay $ 1 * 1000 * 1000

  timelineUpdateThread twInfo refreshInterval chan next
