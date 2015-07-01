{-# LANGUAGE OverloadedStrings #-}
module App
  ( app
  , initialAppState
  , refreshInterval
  )
where

import Brick.Main (App(..), showFirstCursor)
import Brick.Widgets.Edit (editor)
import Brick.Widgets.Core (str)

import Types
import UI
import EventHandlers

refreshInterval :: Integer
refreshInterval = 120

app :: App St TootEvent
app =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleAppEvent
        , appStartEvent = handleStartEvent
        , appAttrMap = const theAttrMap
        , appMakeVtyEvent = VtyEvent
        }

initialAppState :: St
initialAppState =
    St { _timeline = []
       , _editTweet = editor "editTweet" str ""
       , _spinnerState = Nothing
       , _notification = Nothing
       , _help = []
       }

