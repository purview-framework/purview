{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
module ComponentSpec where

import Prelude hiding (div)
import Control.Concurrent.STM.TChan
import Test.Hspec
import Data.Aeson
import Data.Aeson.TH
import Data.Time

import Component
import Events

data TestAction = Up | Down

$(deriveJSON defaultOptions ''TestAction)

data SingleConstructor = SingleConstructor

$(deriveJSON (defaultOptions{tagSingleConstructors=True}) ''SingleConstructor)

spec :: SpecWith ()
spec = parallel $ do

  describe "render" $ do

    it "can create a div" $ do
      let element = Html "div" [Text "hello world"]

      render element `shouldBe` "<div>hello world</div>"

    it "can add an onclick" $ do
      let element =
            Attribute (OnClick (1 :: Integer))
            $ Html "div" [Text "hello world"]

      render element `shouldBe`
        "<div action=1>hello world</div>"

    it "can add an id" $ do
      let element =
            identifier "hello" $ div [text "it's a hello div"]
      render element `shouldBe` "<div id=\"hello\">it's a hello div</div>"

    it "can render a message handler" $ do
      let
        actionHandler :: String -> Int -> Int
        actionHandler "up" _ = 1
        actionHandler _    _ = 0

        handler =
          messageHandler (0 :: Int)
            actionHandler
            (Text . show)

      render handler
        `shouldBe`
        "<div handler=\"null\">0</div>"

    it "can render a typed action" $ do
      let element = onClick SingleConstructor $ div [ text "click" ]

      render element
        `shouldBe`
        "<div action=\"SingleConstructor\">click</div>"

    it "can render a style" $ do
      let element = style "color: blue;" $ div [ text "blue" ]

      render element
        `shouldBe`
        "<div style=\"color: blue;\">blue</div>"

    it "can render composed styles" $ do
      let blue = style "color: blue;"
          halfSize = style "width: 50%; height: 50%;"

      render (blue . halfSize $ div [ text "box" ])
        `shouldBe`
        "<div style=\"width: 50%; height: 50%;color: blue;\">box</div>"

  describe "applyEvent" $ do

    it "changes state" $ do
      let
        actionHandler :: String -> Int -> Int
        actionHandler "up" _ = 1
        actionHandler _    _ = 0

        handler =
          messageHandler (0 :: Int)
            actionHandler
            (Text . show)

      render handler
        `shouldBe`
        "<div handler=\"null\">0</div>"

      chan <- newTChanIO

      let event' = FromEvent { event="click", message="up", location=Nothing }

      appliedHandler <- applyEvent chan event' handler

      render appliedHandler
        `shouldBe`
        "<div handler=\"null\">1</div>"

    it "works with typed messages" $ do
      let
        actionHandler :: TestAction -> Int -> Int
        actionHandler Up   _ = 1
        actionHandler Down _ = 0

        handler =
          messageHandler (0 :: Int)
            actionHandler
            (Text . show)

      render handler
        `shouldBe`
        "<div handler=\"null\">0</div>"

      chan <- newTChanIO

      let event' = FromEvent { event="click", message=toJSON Up, location=Nothing }

      appliedHandler <- applyEvent chan event' handler

      render appliedHandler
        `shouldBe`
        "<div handler=\"null\">1</div>"

    it "works after sending an event that did not match anything" $ do
      let
        actionHandler :: TestAction -> Int -> Int
        actionHandler Up   _ = 1
        actionHandler Down _ = 0

        handler =
          messageHandler (0 :: Int)
            actionHandler
            (Text . show)

      chan <- newTChanIO

      let event0 = FromEvent { event="init", message="init", location=Nothing }

      appliedHandler0 <- applyEvent chan event0 handler
      render appliedHandler0
        `shouldBe`
        "<div handler=\"null\">0</div>"

      let event1 = FromEvent { event="init", message=toJSON Up, location=Nothing }

      appliedHandler1 <- applyEvent chan event1 appliedHandler0
      render appliedHandler1
        `shouldBe`
        "<div handler=\"null\">1</div>"


  describe "prepareGraph" $ do

    it "sets hasRun to True" $ do
      let
        display time = div
          [ text (show time)
          , onClick ("setTime" :: String) $ div [ text "check time" ]
          ]

        startClock cont state = Once (\send -> send ("setTime" :: String)) False (cont state)

        timeHandler = EffectHandler Nothing Nothing handle
          where
            handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime)
            handle "setTime" _ = Just <$> getCurrentTime
            handle _ state     = pure state

        component = timeHandler (startClock display)

      show (fst (prepareGraph component))
        `shouldBe`
        "EffectHandler Just [] Once True div [  \"Nothing\" Attr div [  \"check time\" ]  ] "

      length (snd (prepareGraph component))
        `shouldBe`
        1

    it "stops collecting the action if it has already run" $ do
      let
        display time = div
          [ text (show time)
          , onClick ("setTime" :: String) $ div [ text "check time" ]
          ]

        startClock cont state = Once (\send -> send ("setTime" :: String)) False (cont state)

        timeHandler = EffectHandler Nothing Nothing handle
          where
            handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime)
            handle "setTime" _ = Just <$> getCurrentTime
            handle _ state     = pure state

        component = timeHandler (startClock display)

      let
        run1 = prepareGraph component
        run2 = prepareGraph (fst run1)
        run3 = prepareGraph (fst run2)

      length (snd run1) `shouldBe` 1
      length (snd run2) `shouldBe` 0
      length (snd run3) `shouldBe` 0  -- for a bug where it was resetting run

    it "assigns a location to handlers" $ do
      let
        timeHandler = effectHandler Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime)
        handle "setTime" _     = Just <$> getCurrentTime
        handle _         state = pure state

        component = timeHandler (const (Text ""))

      component `shouldBe` EffectHandler Nothing Nothing handle (const (Text ""))

      let
        graphWithLocation = fst (prepareGraph component)

      graphWithLocation `shouldBe` EffectHandler (Just []) Nothing handle (const (Text ""))

    it "assigns a different location to child handlers" $ do
      let
        timeHandler = effectHandler Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime)
        handle "setTime" _     = Just <$> getCurrentTime
        handle _         state = pure state

        component = div
          [ timeHandler (const (Text ""))
          , timeHandler (const (Text ""))
          ]

        graphWithLocation = fst (prepareGraph component)

      show graphWithLocation `shouldBe` "div [  EffectHandler Just [0] \"\" EffectHandler Just [1] \"\" ] "

    it "assigns a different location to nested handlers" $ do
      let
        timeHandler = effectHandler Nothing handle

        handle :: String -> Maybe UTCTime -> IO (Maybe UTCTime)
        handle "setTime" _     = Just <$> getCurrentTime
        handle _         state = pure state

        component =
          timeHandler (const (timeHandler (const (Text ""))))


        graphWithLocation = fst (prepareGraph component)

      show graphWithLocation `shouldBe` "EffectHandler Just [] EffectHandler Just [0] \"\""

main :: IO ()
main = hspec spec
