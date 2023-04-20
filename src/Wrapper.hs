{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Wrapper where

import           Text.RawString.QQ (r)
import           Data.Text (Text)


data HtmlEventHandler = HtmlEventHandler
  { eventType :: String -- eg submit or click
  , functionName :: String -- called whenever the event happens
  , handlingFunction :: String -- receives the event and sends the event over the websocket
  }


{-

Loosely, for each type of event, check if it has a "clickLocation" "blurLocation" etc
Each event would have its own location and if it doesn't, send nothing

I think that actually does it?

Might also want to move to using "clickId" etc, if you want to.

-}

clickEventHandlingFunction :: String
clickEventHandlingFunction = [r|
  function handleClickEvents(event) {
    event.stopPropagation();

    var clickValue;
    try {
      clickLocation = JSON.parse(event.target.getAttribute("location"));
    } catch (error) {
      // if the action is just a string, parsing it as JSON would fail
      clickLocation = event.target.getAttribute("location");
    }
    var location = JSON.parse(event.currentTarget.getAttribute("handler"))

    if (clickLocation) {
      window.ws.send(JSON.stringify({ "event": "click", "childLocation": clickLocation, "location": location }));
    }
  }
|]

clickEventHandler :: HtmlEventHandler
clickEventHandler = HtmlEventHandler "click" "handleClickEvents" clickEventHandlingFunction

blurEventHandlingFunction :: String
blurEventHandlingFunction = [r|
  function handleBlurEvents(event) {
    event.stopPropagation();

    var blurValue;
    try {
      blurLocation = JSON.parse(event.target.getAttribute("location"));
    } catch (error) {
      // if the action is just a string, parsing it as JSON would fail
      blurLocation = event.target.getAttribute("location");
    }
    var location = JSON.parse(event.currentTarget.getAttribute("handler"))

    var value = event.target.value;

    if (blurLocation) {
      window.ws.send(JSON.stringify({ "event": "blur", "value": value, "childLocation": blurLocation, "location": location }));
    }
  }
|]

blurEventHandler :: HtmlEventHandler
blurEventHandler = HtmlEventHandler "blur" "handleBlurEvents" blurEventHandlingFunction

changeEventHandlingFunction :: String
changeEventHandlingFunction = [r|
  function handleChangeEvents(event) {
    event.stopPropagation();

    var changeValue;
    try {
      changeLocation = JSON.parse(event.target.getAttribute("location"));
    } catch (error) {
      // if the action is just a string, parsing it as JSON would fail
      changeLocation = event.target.getAttribute("location");
    }
    var location = JSON.parse(event.currentTarget.getAttribute("handler"))

    var value = event.target.value;

    if (changeLocation) {
      window.ws.send(JSON.stringify({ "event": "change", "value": value, "childLocation": changeLocation, "location": location }));
    }
  }
|]

changeEventHandler :: HtmlEventHandler
changeEventHandler = HtmlEventHandler "change" "handleChangeEvents" blurEventHandlingFunction

submitEventHandlingFunction :: String
submitEventHandlingFunction = [r|
  function handleFormEvents(event) {
    event.preventDefault();
    event.stopPropagation();

    var clickValue;
    try {
      clickLocation = JSON.parse(event.target.getAttribute("location"));
    } catch (error) {
      // if the action is just a string, parsing it as JSON would fail
      clickLocation = event.target.getAttribute("location");
    }

    var form = new FormData(event.target);
    var entries = JSON.stringify(Object.fromEntries(form.entries()));
    var location = JSON.parse(event.currentTarget.getAttribute("handler"))

    if (entries) {
      window.ws.send(JSON.stringify({ "event": "submit", "value": entries, "childLocation": clickLocation, "location": location }));
    }
  }
|]

submitEventHandler :: HtmlEventHandler
submitEventHandler = HtmlEventHandler "submit" "handleFormEvents" submitEventHandlingFunction

defaultHtmlEventHandlers :: [HtmlEventHandler]
defaultHtmlEventHandlers =
  [ clickEventHandler
  , blurEventHandler
  , changeEventHandler
  , submitEventHandler
  ]

mkBinding :: HtmlEventHandler -> String
mkBinding (HtmlEventHandler kind functionName _) =
  "item.removeEventListener(\"" <> kind <> "\", " <>  functionName <> ");"
  <> "item.addEventListener(\"" <> kind <> "\", " <>  functionName <> ");"

mkFunction :: HtmlEventHandler -> String
mkFunction (HtmlEventHandler _ _ function) = function

bindEvents :: [HtmlEventHandler] -> String
bindEvents htmlEventHandlers =
  let bindings = foldr (<>) "" $ fmap mkBinding htmlEventHandlers
      functions = foldr (<>) "" $ fmap mkFunction htmlEventHandlers
  in
    functions
    <> "function bindEvents() {"
    <> "document.querySelectorAll(\"[handler]\").forEach(item => {"
    <> bindings
    <> "});"
    <> "};"

-- TODO: revisit this
-- bindLocations :: String
-- bindLocations = [r|
--   function bindLocations() {
--     const locationAdder = (location) => (event) => {
--       if (!event.target.getAttribute("location")) {
--         event.target.setAttribute("location", location);
--       }
--     }
--
--     document.querySelectorAll("[location]").forEach(item => {
--       item.removeEventListener("click", locationAdder(item.getAttribute("location")));
--       item.addEventListener("click", locationAdder(item.getAttribute("location")));
--     })
--   }
-- |]

websocketScript :: String
websocketScript = [r|
  var timeoutTime = -50;
  function connect() {
    timeoutTime += 50;
    // TODO: adding the current path is kind of a hack
    var ws = new WebSocket("ws://localhost:8001" + window.location.pathname);

    ws.onopen = () => {
      ws.send("initial from js");
      timeoutTime = 0;
    };

    ws.onmessage = evt => {
      var m = evt.data;
      var event = JSON.parse(evt.data);

      if (event.event === "setHtml") {
        // cool enough for now
        event.message.map(command => setHtml(command));
        bindEvents();
        // bindLocations();
      }
    };

    ws.onclose = function() {
      setTimeout(function() {
        console.debug("Attempting to reconnect");
        connect();
      }, timeoutTime);
    };

    window.onbeforeunload = evt => {
      ws.close();
    };

    window.ws = ws;
  }
  connect();

  function getNode(location) {
    let currentNode = document.body;
    while (location.length > 0) {
      const index = location.pop();
      currentNode = currentNode.childNodes[index];
    }
    return currentNode;
  }

  function setHtml(message) {
    const command = message.message;
    const [location, newHtml] = message.contents;
    const targetNode = getNode(location);
    targetNode.outerHTML = newHtml;
  }
|]

wrapHtml :: String -> [HtmlEventHandler] -> String -> String
wrapHtml htmlHead htmlEventHandlers body =
  "<!DOCTYPE html>"
  <> "<html>"
  <> "<head>"
  <> "<script>" <> websocketScript <> bindEvents htmlEventHandlers <> "</script>"
  <> htmlHead
  <> "</head>"
  <> "<body>"
  <> body
  <> "<script>bindEvents(); // bindLocations();</script>"
  <> "</body>"
  <> "</html>"
