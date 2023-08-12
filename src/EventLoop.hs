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
import           Data.Typeable
import           Data.Aeson (encode)
import qualified Network.WebSockets as WebSockets

import           Component
import           Diffing
import           EventHandling
import           Events
import           PrepareTree
import           Rendering
import           CleanTree (cleanTree)
import           CollectInitials (collectInitials)

type Log m = String -> m ()

eventLoop'
  :: (Monad m, Typeable event)
  => Event
  -> Bool
  -> (m [Event] -> IO [Event])
  -> Log IO
  -> TChan Event
  -> WebSockets.Connection
  -> Purview event m
  -> IO (Maybe (Purview event m))
eventLoop' message devMode runner log eventBus connection component = do
  -- if it's special newState event, the state is replaced in the tree
  let newTree = case message of
        FromFrontendEvent {}                 -> component
        InternalEvent {}                     -> component
        JavascriptCallEvent {}               -> component
        stateChangeEvent@StateChangeEvent {} -> applyNewState stateChangeEvent component

  let
    -- 1. adds locations
    locatedTree = prepareTree newTree
    -- 2. collects css and initial events
    (initialEvents, css) = collectInitials locatedTree
    -- 3. removes captured css and initial events
    newTree' = cleanTree css locatedTree
    -- why pass in the found css?  otherwise haskell will optimize by
    -- putting the function that removes css into the tree, which results
    -- in the css being removed before it's been found by collectInitials.
    -- or at least, that's what seemed to be happening.  very funny.

    event = findEvent message newTree'

  -- this is where handlers are actually called, and their events are sent back into
  -- this loop
  void . forkIO $ do
    newEvents <- case (event, message) of
      (_, event'@InternalEvent {}) -> runner $ runEvent event' newTree'
      (Just event', _)             -> runner $ runEvent event' newTree'
      (Nothing,     _)             -> pure []
    mapM_ (atomically . writeTChan eventBus) newEvents

  let
    -- collect diffs
    location = case message of
      (FromFrontendEvent { location }) -> location
      (StateChangeEvent _ location)    -> location
      (InternalEvent { handlerId })    -> handlerId
      (JavascriptCallEvent {})         -> error "tried to get location off javascript call"

    diffs = diff location [0] component newTree'
    -- for now it's just "Update", which the javascript handles as replacing
    -- the html beneath the handler.  I imagine it could be more exact, with
    -- Delete / Create events.
    renderedDiffs = fmap (\(Update location graph) -> Update location (render graph)) diffs

  unless (null css) $ WebSockets.sendTextData
    connection
    (encode $ ForFrontEndEvent { event = "setCSS", message = css })

  WebSockets.sendTextData
    connection
    (encode $ ForFrontEndEvent { event = "setHtml", message = renderedDiffs })

  pure (Just newTree')

handleJavascriptCall :: String -> String -> WebSockets.Connection -> IO (Maybe (Purview event m))
handleJavascriptCall name value connection = do
  WebSockets.sendTextData
    connection
    (encode $ ForFrontEndEvent { event = "callJS", message = [name, value] })

  pure Nothing

--
-- This is the main event loop of handling messages from the websocket
--
-- pretty much just get a message, then run the message via the component
-- handler, and then send the "setHtml" back downstream to tell it to replace
-- the html with the new.
--
eventLoop
  :: (Monad m, Typeable event)
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

  newTree <- case message of
    JavascriptCallEvent name value -> handleJavascriptCall name value connection
    _ -> eventLoop' message devMode runner log eventBus connection component

  case newTree of
    Just tree ->
      case message of
        (FromFrontendEvent { kind }) -> do
          when (devMode && kind == "init") $
            WebSockets.sendTextData
            connection
            (encode $ ForFrontEndEvent { event = "setHtml", message = [ Update [] (render tree) ] })

          eventLoop devMode runner log eventBus connection tree
        _ ->
          eventLoop devMode runner log eventBus connection tree
    Nothing -> eventLoop devMode runner log eventBus connection component
