{-# LANGUAGE OverloadedStrings #-}
module UI
  ( drawUI
  , theAttrMap
  )
where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Data.Default
import qualified Graphics.Vty as V
import Web.Twitter.Types.Lens

import Brick.AttrMap
import Brick.Util
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Types

-- The max length for a twitter username is 15 characters. We limit to
-- 16 to allow for padding.
nickColumnWidth :: Int
nickColumnWidth = 16

maxTweetLength :: Int
maxTweetLength = 140

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
       padRight Max $
       (hLimit nickColumnWidth (padLeft Max nick)) <+> ": " <+> (txt tweet)

drawTimeline :: [Status] -> Widget
drawTimeline ss = viewport "timeline" Vertical $ vBox $ mkTimelineEntry <$> ss

drawUI :: St -> [Widget]
drawUI st = [withBorderStyle unicode ui]
    where
        nick = "@jtdaugherty"
        paddedNick = withAttr "nickname" nick

        editUi = vLimit 1 $ paddedNick <+> ": " <+> (withDefaultAttr tweetEditAttr $ renderEditor (st^.editTweet))

        tweetEditAttr = if (length $ getEditContents $ st^.editTweet) <= maxTweetLength
                        then tweetEditOkay
                        else tweetEditTooLong

        tweetLengthAttr = if (length $ getEditContents $ st^.editTweet) <= maxTweetLength
                          then def
                          else tweetLengthTooLong

        ui = header
             <=> hBorder
             <=> drawTimeline (st^.timeline)
             <=> footer
             <=> editUi
        footer = withAttr "footer" $
               hLimit 1 hBorder
               <+> "[help text here]"
               <+> hBorder
               <+> characterCount
               <+> hLimit 1 hBorder
        characterCount = withAttr tweetLengthAttr $
               (str $ show $ length $ getEditContents $ st^.editTweet)
               <+> "/"
               <+> (str $ show maxTweetLength)
        header =  withAttr "header" "toot!"
                  <+> (vLimit 1 $ fill ' ')
                  <+> currentNotify
                  <+> " "
                  <+> spinner
        currentNotify = case st^.notification of
            Just (aName, t) -> withAttr aName $ txt t
            Nothing -> txt " "
        spinner = case st^.spinnerState of
            Nothing -> emptyWidget
            Just c -> str [c]
