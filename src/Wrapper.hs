{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Wrapper where

import           Text.RawString.QQ (r)
import           Data.Text (Text)

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
        document.body.innerHTML = event.message;
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

  function handleEvents(event) {
    var clickValue = event.target.getAttribute("bridge-click");
    if (clickValue) {
      window.ws.send(JSON.stringify({ "event": "click", "message": clickValue }));
    }
  }

  function bindEvents() {
    document.getRootNode().addEventListener("click", handleEvents);
  }
  bindEvents();
|]

wrapHtml :: Text -> Text
wrapHtml body =
  "<html>"
  <> "<head>"
  <> "<script>" <> websocketScript <> "</script>"
  <> "</head>"
  <> "<body>"<> body <> "</body>"
  <> "</html>"
