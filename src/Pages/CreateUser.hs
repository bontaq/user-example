{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Pages.CreateUser (render) where

import Prelude hiding ( div )
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, decode )
import Data.ByteString.Lazy.Char8 ( pack )

import Effectful ( MonadIO(liftIO), type (:>), Eff, IOE )

import Purview hiding ( reducer, render )
import Services.Users (addUser, User (..), UserRepo)

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

-- Just used for parsing the raw form input.  Although I wonder if, with some
-- magic, forms could be generated from data types?
data UserForm = UserForm { name :: String, email :: String, password :: String }
  deriving (Generic, Show)

instance FromJSON UserForm where

-------------
-- Effects --
-------------

type State = ()

newtype Event = CreateUser (Maybe String)
  deriving (Show, Eq)

reducer'
  :: (IOE :> es, UserRepo :> es)
  => Event -> State -> Eff es (State, [DirectedEvent parent Event])
reducer' event _ = case event of

  CreateUser (Just formInput) -> do
    -- parse the JSON from the form into something a little more usable
    let userForm = decode (pack formInput) :: Maybe UserForm

    case userForm of
      Just formData -> do
        -- actually add the user, hurrah!
        users <- addUser (User formData.name formData.email formData.password)
        liftIO $ print users

        pure (() , [])

      Nothing ->
        pure ((), [])

reducer
  :: (IOE :> es, UserRepo :> es)
  => (State -> Purview Event (Eff es)) -> Purview () (Eff es)
reducer = effectHandler'
  []       -- initial actions
  ()       -- initial state
  reducer' -- handles events

render :: (IOE :> es, UserRepo :> es) => Purview () (Eff es)
render = reducer view
