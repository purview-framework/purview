{-# LANGUAGE OverloadedStrings #-}
module EventsSpec where

import Test.Hspec
import Data.Aeson

import Events
import Data.Maybe (isNothing)


spec :: SpecWith ()
spec = parallel $ do

  describe "parsing events" $ do
    it "parses a FrontEndEvent with a value of nothing" $ do
      decode "{ \"event\": \"click\", \"childLocation\": null, \"location\": null }"
        `shouldBe`
        Just (FromFrontendEvent { kind="click", childLocation=Nothing, location=Nothing, value=Nothing })

    it "parses a FrontEndEvent with a string value" $ do
      decode "{ \"event\": \"submit\", \"childLocation\": null, \"location\": null, \"value\": \"hello\" }"
        `shouldBe`
        Just (FromFrontendEvent { kind="submit", childLocation=Nothing, location=Nothing, value=Just "hello" })

    it "parses a FrontEndEvent with a dictionary value" $ do
      decode "{ \"event\": \"submit\", \"childLocation\": null, \"location\": null, \"value\": \"{}\" }"
        `shouldBe`
        Just (FromFrontendEvent { kind="submit", childLocation=Nothing, location=Nothing, value=Just "{}" })


main :: IO ()
main = hspec spec
