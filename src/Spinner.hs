module Spinner
  ( withSpinner
  )
where

import Control.Concurrent (Chan, ThreadId, killThread, threadDelay, forkIO, writeChan)
import Control.Monad (forM_)
import Control.Exception (finally)

import Types

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
