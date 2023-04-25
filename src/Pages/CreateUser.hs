{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Pages.CreateUser (render) where

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy.Char8 (pack)
import GHC.Generics (Generic)
import Prelude hiding (div)

import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))

import Purview hiding (reducer, render)
import Router
import Services.Users (User (..), UserRepo, addUser)

----------
-- View --
----------

typeAttr = Attribute . Generic "type"
nameAttr = Attribute . Generic "name"

textField = typeAttr "text" $ input []

submitButton = typeAttr "submit" $ button [text "submit"]

view :: () -> Purview Event m
view _ =
  div
    [ p [text "Create a user"]
    , onSubmit CreateUser $
        form
          [ div [text "name", nameAttr "name" textField]
          , div [text "email", nameAttr "email" textField]
          , div [text "password", nameAttr "password" textField]
          , submitButton
          ]
    ]

-- Just used for parsing the raw form input.  Although I wonder if, with some
-- magic, forms could be generated from data types?
data UserForm = UserForm {name :: String, email :: String, password :: String}
  deriving (Generic, Show)

instance FromJSON UserForm

-------------
-- Effects --
-------------

type State = ()

newtype Event = CreateUser (Maybe String)
  deriving (Show, Eq)

reducer'
  :: (IOE :> es, UserRepo :> es)
  => Event
  -> State
  -> Eff es (State, [DirectedEvent RouterEvents Event])
reducer' event _ = case event of
  CreateUser (Just formInput) -> do
    -- parse the JSON from the form into something a little more usable
    let userForm = decode (pack formInput) :: Maybe UserForm

    case userForm of
      Just formData -> do
        -- actually add the user, hurrah!
        users <- addUser (User formData.name formData.email formData.password)
        liftIO $ print users

        -- send them back home
        pure ((), [Parent (Redirect "/")])
      Nothing ->
        pure ((), [])

reducer
  :: (IOE :> es, UserRepo :> es)
  => (State -> Purview Event (Eff es))
  -> Purview RouterEvents (Eff es)
reducer =
  effectHandler'
    [] -- initial actions
    () -- initial state
    reducer' -- handles events

render :: (IOE :> es, UserRepo :> es) => Purview RouterEvents (Eff es)
render = reducer view
