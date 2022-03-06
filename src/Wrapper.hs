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

  function handleEvents(event) {
    event.stopPropagation();

    var clickValue = event.target.getAttribute("action");
    var location = JSON.parse(event.currentTarget.getAttribute("handler"))

    if (clickValue) {
      window.ws.send(JSON.stringify({ "event": "click", "message": clickValue, "location": location }));
    }
  }

  function bindEvents() {
    document.querySelectorAll("[handler]").forEach(item => {
      item.removeEventListener("click", handleEvents);
      item.addEventListener("click", handleEvents);
    });
    console.log('events bound');
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
