{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}
module Main where

import Control.Lens
import Control.Concurrent
import Control.Applicative
import Control.Exception
import Control.Monad (forM_)
import Data.Monoid
import Data.Ini
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Web.Authenticate.OAuth
import Web.Twitter.Conduit.Monad
import Web.Twitter.Types.Lens
import System.Exit
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL

import qualified Graphics.Vty as V
import Brick.Core
import Brick.Util
import Brick.Main
import Brick.AttrMap
import Brick.Widgets.Edit
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Twitter

data TootEvent = Notify AttrName T.Text
               | ClearNotify
               | NewTweets [Status]
               | SetSpinner Char
               | StopSpinner
               | VtyEvent V.Event

-- TODO:
-- Replace entities (&amp;)
-- Highlight hashtags, mentions, URLs in tweets and editor
-- Do something with media?
-- Support posting tweets
-- Mark replies to you with a special color
-- Support scrolling through tweets to select one to reply to

-- The max length for a twitter username is 15 characters. We limit to
-- 16 to allow for padding.
nickColumnWidth :: Int
nickColumnWidth = 16

maxTweetLength :: Int
maxTweetLength = 140

refreshInterval :: Integer
refreshInterval = 120

withSpinner :: Chan TootEvent -> IO a -> IO a
withSpinner chan action = do
    pid <- startSpinner chan
    action `finally` (killThread pid)

startSpinner :: Chan TootEvent -> IO ThreadId
startSpinner chan = do
    let states = ['▁', '▃', '▄', '▅', '▆', '▇', '█', '▇', '▆', '▅', '▄', '▃']
        threadBody = do
            forM_ (concat $ repeat states) $ \s -> do
                writeChan chan $ SetSpinner s
                threadDelay $ 1000000 `div` (length states)

    forkIO $ threadBody `finally` (writeChan chan StopSpinner)

timelineUpdateThread :: TWInfo -> Chan TootEvent -> Maybe StatusId -> IO ()
timelineUpdateThread twInfo chan since = do
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

  timelineUpdateThread twInfo chan next

data St =
    St { _timeline :: [Status]
       , _editTweet :: Editor
       , _spinnerState :: Maybe Char
       , _notification :: Maybe (AttrName, T.Text)
       , _help :: [(T.Text, T.Text)]
       }

makeLenses ''St

tweetLengthTooLong :: AttrName
tweetLengthTooLong = "tweetTooLong"

tweetEditOkay :: AttrName
tweetEditOkay = "tweetEditOkay"

tweetEditTooLong :: AttrName
tweetEditTooLong = "tweetEditTooLong"

theAttrMap :: AttrMap
theAttrMap = attrMap (V.white `on` V.black)
    [ ("notification",          fg V.cyan)
    , ("error",                 fg V.red)
    , ("header",                fg V.cyan)
    , ("nickname",              fg V.magenta)
    , (tweetEditOkay,           fg V.cyan)
    , (tweetEditTooLong,        V.white `on` V.red)
    , ("retweet",               V.yellow `on` V.blue)
    , (tweetLengthTooLong,      fg V.red)
    ]

appEvent :: TootEvent -> St -> EventM (Next St)
appEvent e st =
    let timelineScroll = viewportScroll "timeline"
    in case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt st
        VtyEvent (V.EvKey V.KDown []) -> do
            scrollBy timelineScroll 1
            continue st
        VtyEvent (V.EvKey V.KUp []) -> do
            scrollBy timelineScroll (-1)
            continue st

        -- TODO: on Enter keypress, do nothing if the tweet is too long
        VtyEvent kev@(V.EvKey _ _) -> continue $ st & editTweet %~ (handleEvent kev)

        Notify aName t -> continue $ st & notification .~ Just (aName, t)
        ClearNotify -> continue $ st & notification .~ Nothing
        NewTweets ss -> do
            scrollToEnd timelineScroll
            continue $ st & timeline %~ (++ reverse ss)
        SetSpinner ch -> continue $ st & spinnerState .~ Just ch
        StopSpinner -> continue $ st & spinnerState .~ Nothing
        _ -> continue st

mkTimelineEntry :: Status -> Widget
mkTimelineEntry st =
    let isRT = case st^.statusRetweetedStatus of
                 Nothing -> False
                 Just _ -> True
        uname = st^.statusUser.userScreenName
        tweet = st^.statusText
        tweetAttr = if isRT then "retweet" else def
        nick = withAttr "nickname" $ txt uname

    in withDefaultAttr tweetAttr $
       padRight $
       (hLimit nickColumnWidth (padLeft nick)) <+> ": " <+> (txt tweet)

drawTimeline :: [Status] -> Widget
drawTimeline ss = viewport "timeline" Vertical $ vBox $ mkTimelineEntry <$> ss

drawUI :: St -> [Widget]
drawUI st = [withBorderStyle unicode ui]
    where
        nick = "@jtdaugherty"
        paddedNick = withAttr "nickname" nick

        editUi = vLimit 1 $ paddedNick <+> ": " <+> (withDefaultAttr tweetEditAttr $ renderEditor (st^.editTweet))

        tweetEditAttr = if (length $ st^.editTweet.editContentsL) <= maxTweetLength
                        then tweetEditOkay
                        else tweetEditTooLong

        tweetLengthAttr = if (length $ st^.editTweet.editContentsL) <= maxTweetLength
                          then def
                          else tweetLengthTooLong

        ui = vBox [ hBox [ withAttr "header" "toot!"
                         , vLimit 1 $ fill ' '
                         , case st^.notification of
                             Just (aName, t) -> withAttr aName $ txt t
                             Nothing -> txt " "
                         , " "
                         , case st^.spinnerState of
                             Nothing -> emptyWidget
                             Just c -> str [c]
                         ]
                  , hBorder
                  , drawTimeline (st^.timeline)
                  , withAttr "footer" $
                      hBox [ hLimit 1 hBorder
                           , "[help text here]"
                           , hBorder
                           , withAttr tweetLengthAttr $
                             hBox [ str $ show $ length $ st^.editTweet.editContentsL
                                  , "/"
                                  , str $ show maxTweetLength
                                  ]
                           , hLimit 1 hBorder
                           ]
                  , editUi
                  ]

app :: App St TootEvent
app =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appAttrMap = const theAttrMap
        , appMakeVtyEvent = VtyEvent
        }

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

        initSt = St { _timeline = []
                    , _editTweet = editor "editTweet" str ""
                    , _spinnerState = Nothing
                    , _notification = Nothing
                    , _help = []
                    }

    -- Attempt to read tweet cache
    mCacheData <- readTweetCache
    let cacheData = maybe [] id mCacheData
        lastStatus = if null cacheData
                     then Nothing
                     else Just $ (last cacheData)^.statusId

    timelineUpdateChan <- newChan
    forkIO $ timelineUpdateThread twInfo timelineUpdateChan lastStatus
    finalState <- customMain (V.mkVty def) timelineUpdateChan app $ initSt & timeline .~ cacheData

    -- Attempt to read tweet cache
    writeTweetCache $ finalState^.timeline
