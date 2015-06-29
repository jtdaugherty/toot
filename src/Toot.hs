{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}
module Main where

import Control.Lens
import Control.Concurrent
import Control.Applicative
import Control.Exception
import Control.Monad (forM_, void)
import Data.Ini
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Web.Authenticate.OAuth
import Web.Twitter.Conduit.Monad
import Web.Twitter.Types.Lens
import System.Exit

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

nickColumnWidth :: Int
nickColumnWidth = 20

timelineUpdateThread :: TWInfo -> Chan TootEvent -> IO ()
timelineUpdateThread twInfo chan = updateTimeline twInfo chan Nothing

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

updateTimeline :: TWInfo -> Chan TootEvent -> Maybe StatusId -> IO ()
updateTimeline twInfo chan since = do
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
                return Nothing

  next <- doFetch `catch` \(e::TwitterError) -> do
      case e of
          FromJSONError _ -> writeChan chan $ Notify "error" "JSON error"
          TwitterStatusError _ _ _ -> writeChan chan $ Notify "error" "Status error"
          TwitterUnknownErrorResponse _ _ _ -> writeChan chan $ Notify "error" "Unknown twitter error"
          TwitterErrorResponse _ _ ms -> do
              writeChan chan $ Notify "error" (T.concat $ twitterErrorMessage <$> ms)
              threadDelay 10000000
      return since

  forM_ [60,59..1] $ \(s::Integer) -> do
      writeChan chan $ Notify "notification" $ T.concat ["Reloading in ", T.pack $ show s, " sec"]
      threadDelay $ 1 * 1000 * 1000

  updateTimeline twInfo chan $ next <|> since

data St =
    St { _timeline :: [Status]
       , _editTweet :: Editor
       , _spinnerState :: Maybe Char
       , _notification :: Maybe (AttrName, T.Text)
       , _help :: [(T.Text, T.Text)]
       }

makeLenses ''St

theAttrMap :: AttrMap
theAttrMap = attrMap (V.white `on` V.black)
    [ ("notification",          fg V.green)
    , ("error",                 fg V.red)
    , ("header",                fg V.cyan)
    , ("nickname",              fg V.magenta)
    , (editAttr,                fg V.cyan)
    , ("retweet",               V.yellow `on` V.blue)
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
    let (isRT, uname, tweet) =
            case st^.statusRetweeted of
              Nothing -> ( False
                         , st^.statusUser.userScreenName
                         , T.intercalate (T.pack " / ") $
                           filter (not . T.null) $ T.lines $ st^.statusText
                         )
              Just True -> let status = T.concat [ st^.statusText
                                                 , " (via @"
                                                 , st^.statusUser.userScreenName
                                                 , ")"
                                                 ]
                           in ( True
                              , st^.statusUser.userScreenName
                              , T.unlines $ take 2 $ T.lines status
                              )
              Just False -> let status = st^.statusText
                            in ( False
                               , st^.statusUser.userScreenName
                               , T.unlines $ take 2 $ T.lines status
                               )

        tweetAttr = if isRT then "retweet" else def
        nick = withAttr "nickname" $ txt uname
        msg = withAttr tweetAttr $ padRight $ txt tweet
        paddedNick = padLeft nick

    in (hLimit nickColumnWidth paddedNick) <+> ": " <+> msg

drawTimeline :: [Status] -> Widget
drawTimeline [] = fill ' '
drawTimeline ss = viewport "timeline" Vertical $ vBox $ mkTimelineEntry <$> ss

drawUI :: St -> [Widget]
drawUI st = [withBorderStyle unicode ui]
    where
        nick = "@jtdaugherty"
        paddedNick = withAttr "nickname" nick

        editUi = vLimit 1 $ paddedNick <+> ": " <+> renderEditor (st^.editTweet)

        ui = vBox [ hBox [ withAttr "header" "toot!"
                         , vLimit 1 $ fill ' '
                         , case st^.notification of
                             Just (aName, t) -> withAttr aName $ txt t
                             Nothing -> txt " "
                         , " "
                         , case st^.spinnerState of
                             Nothing -> str " "
                             Just c -> str [c]
                         ]
                  , hBorder
                  , drawTimeline (st^.timeline)
                  , withAttr "footer" $
                      hBox [ hLimit 1 hBorder
                           , "[help text here]"
                           , hBorder
                           , str $ show $ length $ st^.editTweet.editContentsL
                           , "/140"
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

    timelineUpdateChan <- newChan
    forkIO $ timelineUpdateThread twInfo timelineUpdateChan
    void $ customMain (V.mkVty def) timelineUpdateChan app initSt
