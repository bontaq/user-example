{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Pages.Home (render) where

import Prelude hiding (div)

import Effectful
import Purview hiding (reducer, render)
import Router
import Services.Users

----------
-- View --
----------

viewUser :: User -> Purview a m
viewUser (User {name, email, password}) =
  div [p [text $ name <> " " <> email <> " " <> password]]

view :: [User] -> Purview Event m
view users =
  div
    [ p [text "Users:"]
    , div $ fmap viewUser users
    , onClick CreateUser $ button [text "create user"]
    , onClick LoadUsers $ button [text "load users"]
    ]

-------------
-- Effects --
-------------

type State = [User]

data Event
  = LoadUsers
  | CreateUser
  deriving (Show, Eq)

reducer' :: (UserRepo :> es) => Event -> State -> Eff es (State, [DirectedEvent RouterEvents Event])
reducer' event state = case event of
  LoadUsers -> do
    users <- getUsers
    pure (users, [])
  CreateUser ->
    pure (state, [Parent (Redirect "/create-user")])

reducer :: (UserRepo :> es) => (State -> Purview Event (Eff es)) -> Purview RouterEvents (Eff es)
reducer =
  effectHandler'
    [Self LoadUsers] -- initial actions
    [] -- initial state
    reducer' -- handles events

render :: (UserRepo :> es) => Purview RouterEvents (Eff es)
render = reducer view
