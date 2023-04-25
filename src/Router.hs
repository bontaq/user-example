module Router
  ( routerReducer
  , RouterEvents (..)
  )
where

import Effectful
import Purview hiding ( reducer )

newtype RouterEvents = Redirect String
  deriving (Show, Eq)

reducer :: RouterEvents -> String -> (String, [DirectedEvent () RouterEvents])
reducer (Redirect newPath) _ = (newPath, [])

routerReducer :: String -> (String -> Purview RouterEvents (Eff es)) -> Purview () (Eff es)
routerReducer path = handler'
  []      -- initial events
  path    -- initial state
  reducer -- handles events
