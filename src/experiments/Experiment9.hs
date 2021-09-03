module Experiment9 where

import Control.Applicative
import Control.Concurrent

-- so we have a new approach that's weirder
-- when each part is done rendering, it tells the parent?
-- that it changed.  and so on?

-- or should it be telling some control?

-- lmao is this literally thesis antithesis synthesis

component :: (IO String -> IO ()) -> IO ()
component out = do
  let f = do
        threadDelay 1000000
        pure "done"
  out (pure "loading")
  out f

-- it can have many parents
-- it can have different props
-- ideally it only runs... once
-- I do not think that's possible

-- so then it runs once per location, and something
-- like a cache would be used for shared http requests
-- that seems acceptable to me
