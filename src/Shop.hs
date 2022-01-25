{-# LANGUAGE OverloadedStrings #-}
-- | Can we do an add to cart button that loads

module Shop where

import           Prelude      hiding (div)
import           Data.Text    hiding (count)
import           Lib

--data Effect = Loading | Normal | Disabled
--  deriving Show
--
--data Shop = Shop
--  { addToCartButton :: Effect
--  , item            :: String
--  } deriving Show
--
--data Cart = Cart
--  { items  :: [String]
--  , status :: Effect
--  , open   :: Bool
--  } deriving Show
--
--data State = State
--  { shop :: Shop
--  , cart :: Cart
--  } deriving Show
--
---- it goes like:
---- click add to cart
---- -> show add to cart as loading
---- -> cart state is updated
---- -> call show cart
--defaultState = State
--  { shop = Shop { addToCartButton = Normal, item = "123" }
--  , cart = Cart { items = [], status = Normal, open = False }
--  }
--
--cartState = Cart { items = [], status = Normal, open = False }
--
--cartModal :: Component Cart String
--cartModal = Component
--  { state    = cartState
--  , handlers = \state messages -> state
--  , render   = \state -> div [] []
--  }
--
--data ShopPageEvents = AddToCart
--
--shopPageHandlers state message = case message of
--  AddToCart -> state
--
--shopPageRender state =
--  div []
--    [ div [] [ text $ "The lovely item: " <> (item . shop $ state) ]
--    , div [ onClick AddToCart ] [ text "add to cart" ]
--    , div [] [ SomeComponent cartModal ]
--    ]
--
--shopPage = Component
--  { state = defaultState
--  , handlers = shopPageHandlers
--  , render = shopPageRender }
