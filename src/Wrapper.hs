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

eventHandling :: String
eventHandling = [r|
  function eventHandler(event) {
    event.stopPropagation();

    const type = event.type;
    // ie "clickLocation" or "blurLocation"
    const locationCheck = `${type}Location`;

    console.log(event);
    console.log(type);

    const possibleLocation = event.target.getAttribute(locationCheck);

    if (possibleLocation) {
      const childLocation = JSON.parse(possibleLocation)

      if (type === "submit") {
        event.preventDefault();

        var form = new FormData(event.target);
        var entries = JSON.stringify(Object.fromEntries(form.entries()));
        var location = JSON.parse(event.currentTarget.getAttribute("handler"));

        window.ws.send(JSON.stringify({
          "event": "submit",
          "value": entries,
          "childLocation": childLocation,
          "location": location
        }));
      } else {
        var value = event.target.value;
        var location = JSON.parse(event.currentTarget.getAttribute("handler"))

        window.ws.send(JSON.stringify({
          "event": "submit",
          "value": value,
          "childLocation": childLocation,
          "location": location
        }));
      }
    }
  }

  const events = ["click", "focusout", "focusin", "change", "submit"];

  function bindEvents() {
    document.querySelectorAll("[handler]").forEach(item => {
      if (!item.getAttribute("bound")) {
        events.map(event => {
          item.addEventListener(event, eventHandler)
        })
        item.setAttribute("bound", "true")
      }
    })
  }
|]

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
defaultHtmlEventHandlers = [ submitEventHandler ]

mkBinding :: HtmlEventHandler -> String
mkBinding (HtmlEventHandler kind functionName _) =
  "item.removeEventListener(\"" <> kind <> "\", " <>  functionName <> ");"
  <> "item.addEventListener(\"" <> kind <> "\", " <>  functionName <> ");"

mkFunction :: HtmlEventHandler -> String
mkFunction (HtmlEventHandler _ _ function) = function

-- bindEvents :: [HtmlEventHandler] -> String
-- bindEvents htmlEventHandlers =
--   let bindings = foldr (<>) "" $ fmap mkBinding htmlEventHandlers
--       functions = foldr (<>) "" $ fmap mkFunction htmlEventHandlers
--   in
--     functions
--     <> "function bindEvents() {"
--     <> "document.querySelectorAll(\"[handler]\").forEach(item => {"
--     <> bindings
--     <> "});"
--     <> "};"

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
  <> "<script>" <> websocketScript <> eventHandling <> "</script>"
  <> htmlHead
  <> "</head>"
  <> "<body>"
  <> body
  <> "<script>bindEvents(); // bindLocations();</script>"
  <> "</body>"
  <> "</html>"
