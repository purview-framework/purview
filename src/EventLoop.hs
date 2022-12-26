{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module EventLoop
 ( eventLoop )
where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Monad
import           Control.Concurrent
import           Data.Aeson (encode)
import qualified Network.WebSockets as WebSockets

import           Component
import           Diffing
import           EventHandling
import           Events
import           PrepareTree
import           Rendering

type Log m = String -> m ()

--
-- This is the main event loop of handling messages from the websocket
--
-- pretty much just get a message, then run the message via the component
-- handler, and then send the "setHtml" back downstream to tell it to replace
-- the html with the new.
--
eventLoop
  :: Monad m
  => Bool
  -> (m [Event] -> IO [Event])
  -> Log IO
  -> TChan Event
  -> WebSockets.Connection
  -> Purview event m
  -> IO ()
eventLoop devMode runner log eventBus connection component = do
  message <- atomically $ readTChan eventBus

  when devMode $ log $ "received> " <> show message

  let
    -- this collects any actions that should run once and sets them
    -- to "run" in the tree, while assigning locations / identifiers
    -- to the event handlers
    newTree = addLocations component

  -- if it's special newState event, the state is replaced in the tree
  let newTree' = case message of
        Event {} -> newTree
        stateChangeEvent -> applyNewState stateChangeEvent newTree

  -- this is where handlers are actually called, and their events are sent back into
  -- this loop
  void . forkIO $ do
    newEvents <- runner $ runEvent message newTree'
    mapM_ (atomically . writeTChan eventBus) newEvents

  -- TODO: restore when changing handlers
  -- mapM_ (atomically . writeTChan eventBus) actions

  let
    -- collect diffs
    location = case message of
      (Event { location }) -> location
      (StateChangeEvent _ location) -> location

    diffs = diff location [0] component newTree'
    -- for now it's just "Update", which the javascript handles as replacing
    -- the html beneath the handler.  I imagine it could be more exact, with
    -- Delete / Create events.
    renderedDiffs = fmap (\(Update location graph) -> Update location (render graph)) diffs

  when devMode $ log $ "sending> " <> show renderedDiffs

  WebSockets.sendTextData
    connection
    (encode $ ForFrontEndEvent { event = "setHtml", message = renderedDiffs })

  case message of
    (Event { event }) ->
      when (devMode && event == "init") $
        WebSockets.sendTextData
          connection
          (encode $ ForFrontEndEvent { event = "setHtml", message = [ Update [] (render newTree') ] })
    _ -> pure ()

  eventLoop devMode runner log eventBus connection newTree'
