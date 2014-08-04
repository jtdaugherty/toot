{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}
module Main where

import Control.Lens
import Control.Concurrent (forkIO, threadDelay)
import Control.Applicative
import Control.Exception
import Control.Monad (forM_)
import Data.Ini
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Web.Authenticate.OAuth
import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Types
import Web.Twitter.Types.Lens (StatusId, statusId)
import System.Exit

import Graphics.Vty.Widgets.EventLoop
import Graphics.Vty.Widgets.Core
import Graphics.Vty (green, red, blue)

import Twitter
import UI

timelineUpdateThread :: TWInfo -> App -> IO ()
timelineUpdateThread twInfo app = _updateTimeline twInfo app Nothing

_updateTimeline :: TWInfo -> App -> Maybe StatusId -> IO ()
_updateTimeline twInfo app since = do
  let doFetch = do
                  result <- withSpinner app $ do
                    (app^.notify) green $ T.pack "Fetching..."
                    getHomeTimeline twInfo since

                  case result of
                    Right ss -> do
                        app^.postNewTweets $ ss
                        app^.clearNotify
                        return $ if null ss then Nothing else Just $ (last ss)^.statusId
                    Left e -> do
                        (app^.notify) red $ T.pack $ "Error fetching tweets: " ++ e
                        return Nothing

  next <- doFetch `catch` \(e::TwitterError) -> do
      case e of
          FromJSONError _ -> (app^.notify) red "JSON error"
          TwitterStatusError _ _ _ -> (app^.notify) red "Status error"
          TwitterErrorResponse _ _ ms -> do
              (app^.notify) red $ T.concat $ twitterErrorMessage <$> ms
              threadDelay 10000000
      return since

  forM_ [60,59..1] $ \(s::Integer) -> do
      (app^.notify) blue $ T.concat ["Reloading in ", T.pack $ show s, " sec"]
      threadDelay $ 1 * 1000 * 1000

  _updateTimeline twInfo app $ next <|> since

main :: IO ()
main = do
    -- Read configuration file
    config <- readIniFile "toot.cfg"
    ini <- case config of
             Left e -> putStrLn e >> exitFailure
             Right i -> return i

    -- Build configuration
    let cfg = unIni ini
        consumerCfg = cfg H.! "consumer"
        tokenCfg = cfg H.! "token"
        textToBS = BS.pack . T.unpack
        oauthData = twitterOAuth { oauthConsumerKey = textToBS $ consumerCfg H.! "oauth_consumer_key"
                                 , oauthConsumerSecret = textToBS $ consumerCfg H.! "oauth_consumer_secret"
                                 }
        creds = Credential [ ("oauth_token", textToBS $ tokenCfg H.! "oauth_token")
                           , ("oauth_token_secret", textToBS $ tokenCfg H.! "oauth_token_secret")
                           ]
        twInfo = def { twToken = def { twOAuth = oauthData
                                     , twCredential = creds
                                     }
                     , twProxy = Nothing
                     }

    (app, c) <- buildUI
    forkIO $ timelineUpdateThread twInfo app
    runUi c defaultContext
