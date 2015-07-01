{-# LANGUAGE TemplateHaskell #-}
module Types
  ( St(..)
  , timeline
  , editTweet
  , spinnerState
  , notification
  , help
  , TootEvent(..)
  )
where

import Control.Lens (makeLenses)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Web.Twitter.Types.Lens

import Brick.Core
import Brick.AttrMap
import Brick.Widgets.Edit

data St =
    St { _timeline :: [Status]
       , _editTweet :: Editor
       , _spinnerState :: Maybe Char
       , _notification :: Maybe (AttrName, T.Text)
       , _help :: [(T.Text, T.Text)]
       }

makeLenses ''St

data TootEvent = Notify AttrName T.Text
               | ClearNotify
               | NewTweets [Status]
               | SetSpinner Char
               | StopSpinner
               | VtyEvent V.Event
