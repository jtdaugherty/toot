{-# LANGUAGE OverloadedStrings #-}
module EventHandlers
  ( handleAppEvent
  , handleStartEvent
  )
where

import Control.Lens ((.~), (&), (%~))
import qualified Graphics.Vty as V

import Brick.Main
import Brick.Types

import Types

handleAppEvent :: St -> TootEvent -> EventM (Next St)
handleAppEvent st e =
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

handleStartEvent :: St -> EventM St
handleStartEvent st = do
    let timelineScroll = viewportScroll "timeline"
    scrollToEnd timelineScroll
    return st
