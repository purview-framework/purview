{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module EventLoop
 ( eventLoop )
where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Concurrent
import           Data.Aeson (encode)
import qualified Network.WebSockets as WS

import           Component
import           Diffing
import           Events
import           Rendering

type Log m = String -> m ()

--
-- This is the main event loop of handling messages from the websocket
--
-- pretty much just get a message, then run the message via the component
-- handler, and then send the "setHtml" back downstream to tell it to replace
-- the html with the new.
--
eventLoop :: Monad m => (m [FromEvent] -> IO [FromEvent]) -> Log IO -> TChan FromEvent -> WS.Connection -> Purview a m -> IO ()
eventLoop runner log eventBus connection component = do
  message@FromEvent { event } <- atomically $ readTChan eventBus
  log $ "received> " <> show message

  let
    (newTree, actions) = prepareGraph component

  -- apply can replace state
  let newTree' = case event of
        "newState" -> applyNewState eventBus message newTree
        _          -> newTree

  newEvents <- runner $ runEvent message newTree'

  mapM_ (atomically . writeTChan eventBus) actions
  mapM_ (atomically . writeTChan eventBus) newEvents

  let
    -- so I think here, we diff then render the diffs
    diffs = diff [0] component newTree'
    renderedDiffs = fmap (\(Update location graph) -> Update location (render graph)) diffs

  -- log $ "new html> " <> show newTree'
  log $ "sending> " <> show renderedDiffs

  WS.sendTextData
    connection
    (encode $ Event { event = "setHtml", message = renderedDiffs })

  eventLoop runner log eventBus connection newTree'
