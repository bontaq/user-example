{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Pages.Home (render) where

import Prelude hiding (div)

import Effectful
import Purview hiding (render, reducer)
import Services.Users

----------
-- View --
----------

viewUser :: User -> Purview a m
viewUser (User { name, email, password }) =
  div [ p [ text $ name <> " " <> email <> " " <> password ] ]

view :: [User] -> Purview a m
view users = div
  [ p [ text "Users:" ]
  , div $ fmap viewUser users
  ]


-------------
-- Effects --
-------------

type State = [User]

data Event = LoadUsers
  deriving (Show, Eq)

reducer' :: UserRepo :> es => Event -> State -> Eff es (State, [DirectedEvent parent Event])
reducer' event _ = case event of
  LoadUsers -> do
    users <- getUsers
    pure (users, [])

reducer :: UserRepo :> es => (State -> Purview Event (Eff es)) -> Purview () (Eff es)
reducer = effectHandler'
  [Self LoadUsers]  -- initial actions
  []                -- initial state
  reducer'          -- handles events

render :: UserRepo :> es => Purview () (Eff es)
render = reducer view
