{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Wrapper where

import           Text.RawString.QQ (r)
import           Data.Text (Text)


data HtmlEventHandler = HtmlEventHandler
  { eventType :: Text -- eg submit or click
  , functionName :: Text -- called whenever the event happens
  , handlingFunction :: Text -- receives the event and sends the event over the websocket
  }

clickEventHandlingFunction :: Text
clickEventHandlingFunction = [r|
  function handleClickEvents(event) {
    event.stopPropagation();

    var clickValue;
    try {
      clickValue = JSON.parse(event.target.getAttribute("action"));
    } catch (error) {
      // if the action is just a string, parsing it as JSON would fail
      clickValue = event.target.getAttribute("action");
    }
    var location = JSON.parse(event.currentTarget.getAttribute("handler"))

    if (clickValue) {
      window.ws.send(JSON.stringify({ "event": "click", "message": clickValue, "location": location }));
    }
  }
|]

clickEventHandler :: HtmlEventHandler
clickEventHandler = HtmlEventHandler "click" "handleClickEvents" clickEventHandlingFunction

submitEventHandlingFunction :: Text
submitEventHandlingFunction = [r|
  function handleFormEvents(event) {
    event.preventDefault();
    event.stopPropagation();

    var form = new FormData(event.target);
    var entries = Object.fromEntries(form.entries());
    var location = JSON.parse(event.currentTarget.getAttribute("handler"))

    if (entries) {
      window.ws.send(JSON.stringify({ "event": "submit", "message": entries, "location": location }));
    }
  }
|]

submitEventHandler :: HtmlEventHandler
submitEventHandler = HtmlEventHandler "submit" "handleFormEvents" submitEventHandlingFunction

htmlEventHandlers :: [HtmlEventHandler]
htmlEventHandlers =
  [ clickEventHandler
  , submitEventHandler
  ]

mkBinding :: HtmlEventHandler -> Text
mkBinding (HtmlEventHandler kind functionName _) =
  "item.removeEventListener(\"" <> kind <> "\", " <>  functionName <> ");"
  <> "item.addEventListener(\"" <> kind <> "\", " <>  functionName <> ");"

mkFunction :: HtmlEventHandler -> Text
mkFunction (HtmlEventHandler _ _ function) = function

bindEvents :: Text
bindEvents =
  let bindings = foldr (<>) "" $ fmap mkBinding htmlEventHandlers
      functions = foldr (<>) "" $ fmap mkFunction htmlEventHandlers
  in
    functions
    <> "function bindEvents() {"
    <> "document.querySelectorAll(\"[handler]\").forEach(item => {"
    <> bindings
    <> "});"
    <> "};"

websocketScript :: Text
websocketScript = [r|
  var timeoutTime = -50;
  function connect() {
    timeoutTime += 50;
    var ws = new WebSocket("ws://localhost:8001");

    ws.onopen = () => {
      ws.send("initial from js");
      timeoutTime = 0;
    };

    ws.onmessage = evt => {
      var m = evt.data;
      console.log( m );
      console.log(JSON.parse( m ));
      var event = JSON.parse(evt.data);
      if (event.event === "setHtml") {
        // cool enough for now
        event.message.map(command => setHtml(command));
        bindEvents();
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

wrapHtml :: Text -> Text
wrapHtml body =
  "<html>"
  <> "<head>"
  <> "<script>" <> websocketScript <> bindEvents <> "bindEvents();" <> "</script>"
  <> "</head>"
  <> "<body>"<> body <> "</body>"
  <> "</html>"
