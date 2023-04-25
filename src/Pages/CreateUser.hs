{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Pages.CreateUser (render) where

import Prelude hiding (div)

import Effectful
import Purview hiding (render, reducer)
import Services.Users (addUser, User, UserRepo)

----------
-- View --
----------

typeAttr = Attribute . Generic "type"
nameAttr = Attribute . Generic "name"

textField = typeAttr "text" $ input []

submitButton = typeAttr "submit" $ button [ text "submit" ]

view :: () -> Purview Event m
view _ = div
  [ p [ text "Create a user" ]
  , onSubmit CreateUser $ form
    [ div [ text "name", nameAttr "name" textField ]
    , div [ text "email", nameAttr "email" textField ]
    , div [ text "password", nameAttr "password" textField ]
    , submitButton
    ]
  ]


-------------
-- Effects --
-------------

type State = ()

data Event = CreateUser (Maybe String)
  deriving (Show, Eq)

reducer'
  :: (IOE :> es, UserRepo :> es)
  => Event -> State -> Eff es (State, [DirectedEvent parent Event])
reducer' event _ = case event of
  CreateUser (Just formInput) -> do
    liftIO $ print formInput
    -- users <- addUser user
    pure (() , [])

reducer
  :: (IOE :> es, UserRepo :> es)
  => (State -> Purview Event (Eff es)) -> Purview () (Eff es)
reducer = effectHandler'
  []       -- initial actions
  ()       -- initial state
  reducer' -- handles events

render :: (IOE :> es, UserRepo :> es) => Purview () (Eff es)
render = reducer view

