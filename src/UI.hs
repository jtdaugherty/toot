{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module UI
    ( App
    , notify
    , clearNotify
    , addToTimeline
    , postNewTweets
    , startSpinner
    , stopSpinner
    , withSpinner
    , timeline
    , buildUI
    , setHelp
    )
where

import Control.Lens
import Control.Applicative
import Control.Exception (finally)
import Data.Monoid
import Control.Concurrent (threadDelay, forkIO, ThreadId, killThread)
import Control.Concurrent.Chan
import Control.Monad (forM_)
import Data.List (intersperse)
import System.Exit
import qualified Data.Text.ICU.Regex as R
import qualified Data.Text as T
import Web.Twitter.Types.Lens hiding (List)
import Graphics.Vty
import Graphics.Vty.Widgets.All
import Graphics.Vty.Widgets.Extras.Text

type StatusListEntry = Box (Box (HLimit (Box HFill FormattedText)) VBorder) FormattedText

type Timeline = List Status StatusListEntry
type MainUI = Box (Box (Box (Box (Box (Box (Box (Box FormattedText HFill) FormattedText) FormattedText) FormattedText) HBorder) Timeline) (Box (Box (Box (Box (Box (Box (Box (Box (Box HBorder FormattedText) FormattedText) FormattedText) (HLimit HBorder)) FormattedText) FormattedText) FormattedText) FormattedText) FormattedText)) (VLimit (Box (Box (HLimit (Box HFill FormattedText)) VBorder) Edit))

data App =
    App { _timeline :: Widget Timeline
        , _statusBar :: Widget FormattedText
        , _editTweet :: Widget Edit
        , _spinner :: Widget FormattedText
        , _helpText :: Widget FormattedText
        , _maxLength :: Widget FormattedText
        , _curLength :: Widget FormattedText
        , _notify :: Color -> T.Text -> IO ()
        , _clearNotify :: IO ()
        , _addToTimeline :: Status -> IO ()
        , _postNewTweets :: [Status] -> IO ()
        , _startSpinner :: IO ()
        , _stopSpinner :: IO ()
        , _setHelp :: [(T.Text, T.Text)] -> IO ()
        }

makeLenses ''App

mkStatusListEntry :: Status -> IO (Widget StatusListEntry)
mkStatusListEntry st = do
    let (uname, tweet) =
            case st^.statusRetweet of
              Nothing -> ( st^.statusUser.userScreenName
                         , T.concat $ intersperse (T.pack " / ") $
                           filter (not . T.null) $ T.lines $ st^.statusText
                         )
              Just t -> let status = T.concat [ t^.statusText
                                              , " (via @"
                                              , st^.statusUser.userScreenName
                                              , ")"
                                              ]
                        in ( t^.statusUser.userScreenName
                           , T.unlines $ take 2 $ T.lines status
                           )

    h <- tweetHighlighter

    nick <- plainText uname
    msg <- plainText tweet

    paddedNick <- hFill ' ' 1 <++> (return nick)

    setTextFormatter nick h
    setTextFormatter msg $ h `mappend` wrap

    w <- (hLimit 20 paddedNick) <++> vBorder <++> (return msg)

    return w

usernamePattern :: T.Text
usernamePattern = "\\@\\w+"

hashTagPattern :: T.Text
hashTagPattern = "\\#\\w+"

urlPattern :: T.Text
urlPattern = "https?\\://[^\\s]+"

editLen :: Int
editLen = 140

buildApp :: IO App
buildApp = do
    sb <- plainText ""
    tl <- buildTimeline
    e <- editWidget
    sp <- plainText " " >>= withNormalAttribute (fgColor green)
    ch <- newChan
    help <- plainText ""
    curL <- plainText "0"
    maxL <- plainText $ T.pack $ show editLen

    setEditMaxLength e $ Just editLen

    e `onChange` (setText curL . T.pack . show . T.length)

    let app = App tl sb e sp help maxL curL
              (\c t -> schedule $ updateSb c t)
              (schedule cl)
              (schedule . updateTl)
              (schedule . postNew)
              (__startSpinner ch sp)
              (__stopSpinner ch)
              updateHelp

        updateHelp pairs = setText help $ T.concat $ intersperse " " $
          ((\(k, n) -> T.concat [k, ":", n]) <$> pairs)

        updateSb c t = setTextWithAttrs sb [(t, fgColor c)]
        updateTl s = (addToList tl s =<< mkStatusListEntry s)
        cl = updateSb white T.empty
        postNew ss = do
          mapM_ updateTl (reverse ss)
          scrollToEnd tl
          cl

    return app

buildTimeline :: IO (Widget Timeline)
buildTimeline = newList 2

withSpinner :: App -> IO a -> IO a
withSpinner app action = do
    app^.startSpinner
    action `finally` (app^.stopSpinner)

tweetHighlighter :: IO Formatter
tweetHighlighter = do
    atRegex <- R.regex [] usernamePattern
    htRegex <- R.regex [] hashTagPattern
    urlRegex <- R.regex [] urlPattern

    return $ mconcat [ highlight atRegex $ fgColor red
                     , highlight urlRegex $ fgColor blue
                     , highlight htRegex $ fgColor white
                     ]

__startSpinner :: Chan ThreadId -> Widget FormattedText -> IO ()
__startSpinner ch w = do
    let states = ["▁", "▃", "▄", "▅", "▆", "▇", "█", "▇", "▆", "▅", "▄", "▃"]
        threadBody = do
            forM_ (concat $ repeat states) $ \s -> do
              schedule $ setText w s
              threadDelay $ 1000000 `div` (length states)

    tid <- forkIO $ threadBody `finally` (schedule $ setText w " ")
    writeChan ch tid

__stopSpinner :: Chan ThreadId -> IO ()
__stopSpinner ch = killThread =<< readChan ch

getLastSelected :: Widget (List a b) -> IO Bool
getLastSelected lst = do
    sel <- getSelected lst
    sz <- getListSize lst
    case sel of
        Nothing -> return True
        Just (i, _) -> return $ i == sz - 1

editHelp :: [(T.Text, T.Text)]
editHelp = [ ("up", "select")
           , ("esc", "quit")
           ]

timelineHelp :: [(T.Text, T.Text)]
timelineHelp = [ ("r", "reply")
               , ("esc", "quit")
               ]

mainUI :: App -> IO (Widget FocusGroup, Widget MainUI)
mainUI app = do
    fg <- newFocusGroup

    addToFocusGroup fg (app^.timeline)
    addToFocusGroup fg (app^.editTweet)

    fg `onKeyPressed` \_ k mods ->
        case (k, mods) of
            (KEsc, []) -> exitSuccess
            _ -> return False

    (app^.editTweet) `onKeyPressed` \_ k mods ->
        case (k, mods) of
          (KUp, []) -> do
              sz <- getListSize $ app^.timeline
              if sz > 0 then (focus $ app^.timeline) >> return True
                        else return False
          _ -> return False

    (app^.timeline) `onKeyPressed` \_ k mods ->
        case (k, mods) of
          (KDown, []) -> do
              v <- getLastSelected $ app^.timeline
              if v then (focus $ app^.editTweet) >> return True else return False
          _ -> return False

    (app^.timeline) `onGainFocus` (const $ (app^.setHelp) timelineHelp)
    (app^.editTweet) `onGainFocus` (const $ (app^.setHelp) editHelp)

    h <- tweetHighlighter
    nick <- plainText "@jtdaugherty"
    paddedNick <- hFill ' ' 1 <++> (return nick)
    setTextFormatter nick h

    editUi <- vLimit 1 =<< (
                 (hLimit 20 paddedNick) <++> vBorder <++> (return $ app^.editTweet)
              )

    ui <- ((plainText "toot twitter client" >>= withNormalAttribute (fgColor cyan))
             <++> (hFill ' ' 1)
             <++> (return $ app^.statusBar))
             <++> plainText " "
             <++> (return $ app^.spinner)
          <--> (hBorder >>= withNormalAttribute (fgColor cyan))
          <--> (return $ app^.timeline)
          <--> ((hBorder
                <++> plainText " "
                <++> (return $ app^.helpText)
                <++> plainText " "
                <++> (hLimit 1 =<< hBorder)
                <++> (plainText " ")
                <++> (return $ app^.curLength)
                <++> (plainText "/")
                <++> (return $ app^.maxLength)
                <++> (plainText " ")
               ) >>= withNormalAttribute (fgColor cyan))
          <--> (return editUi)

    focus $ app^.editTweet

    return (fg, ui)

buildUI :: IO (App, Collection)
buildUI = do
    app <- buildApp
    c <- newCollection
    (mainFg, mainUi) <- mainUI app
    _ <- addToCollection c mainUi mainFg
    return (app, c)
