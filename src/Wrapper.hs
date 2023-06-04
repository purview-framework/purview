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
    // ie "click-location" or "blur-location"
    const locationCheck = `${type}-location`;

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

{-

This is so that event locations are added as events bubble past,
reflecting how you'd expect them to work.

Without this, for example, click events on a div beneath the one
with the click-location would be ignored.

-}
eventBubblingHandling :: String
eventBubblingHandling = [r|
  function bindLocationEnrichment() {
    document.querySelectorAll("[bubbling-bound]").forEach(item => {
      const alreadyBound = item.getAttribute('bubbling-bound')

      if (!alreadyBound) {
        events.map(event => {
          const eventLocation = item.getAttribute(`${event}-location`)

          if (eventLocation) {
            item.addEventListener(event, eventFromDOM => {
              eventFromDOM.target.setAttribute(`${event}-location`, eventLocation)
            })
          }
        })
        item.setAttribute('bubbling-bound', true)
      }
    })
  }
|]

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
        bindLocationEnrichment();
        // bindLocations();
      } else if (event.event === "callJS") {
        const [fnToCall, withValue] = event.message;
        window[fnToCall](withValue);
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

sendEventHelper :: String
sendEventHelper = [r|
  const sendEvent = (receiverName, value) => {
    const targets = document.querySelectorAll("[receiver-name=" + receiverName + "]")
    if (targets.length > 1) {
      console.error("too many")
    } else if (targets.length == 0) {
      console.error("none found")
    } else {
      var target = targets[0]
      var location = JSON.parse(target.getAttribute("parent-handler"))
      var childLocation = JSON.parse(target.getAttribute("handler"))

      window.ws.send(JSON.stringify({
        "event": "submit",
        "value": value,
        "childLocation": childLocation,
        "location": location
      }));
    }
  }
|]

prepareCss :: [(String, String)] -> String
prepareCss = concatMap (\(hash, css) -> "." <> hash <> " {" <> css <> "}")

wrapHtml :: [(String, String)] -> String -> [HtmlEventHandler] -> [String] -> [String] -> String -> String
wrapHtml css htmlHead htmlEventHandlers eventProducers eventListeners body =
  "<!DOCTYPE html>"
  <> "<html>"
  <> "<head>"
  <> "<script>" <> websocketScript <> eventHandling <> eventBubblingHandling <> "</script>"
  <> htmlHead
  <> "<script>"
  <> sendEventHelper
  <> "</script>"
  <> "<script>"
  <> concatMap (<> "\n") eventProducers
  <> "</script>"
  <> "<script>"
  <> concatMap (<> "\n") eventListeners
  <> "</script>"
  <> "<style>"
  <> prepareCss css
  <> "</style>"
  <> "</head>"
  <> "<body>"
  <> body
  <> "<script>bindEvents(); bindLocationEnrichment();</script>"
  <> "</body>"
  <> "</html>"
