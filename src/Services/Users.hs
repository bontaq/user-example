{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Services.Users
  ( getUsers
  , addUser
  , runUserRepoPure
  , UserRepo
  , User (..)
  )
where

import Effectful
import Effectful.Dispatch.Dynamic (reinterpret, send)
import Effectful.State.Static.Local (evalState, gets, modify)


-----------
-- Model --
-----------

data User = User
  { name :: String, email :: String, password :: String }
  deriving (Show, Eq)


-----------------------
-- Effect definition --
-----------------------

data UserRepo :: Effect where
  GetUsers :: UserRepo m [User]
  AddUser  :: User -> UserRepo m [User]

type instance DispatchOf UserRepo = 'Dynamic


---------
-- API --
---------

getUsers :: UserRepo :> es => Eff es [User]
getUsers = send GetUsers

addUser :: UserRepo :> es => User -> Eff es [User]
addUser user = send (AddUser user)


-----------------
-- Interpreter --
-----------------

runUserRepoPure :: [User] -> Eff (UserRepo : es) a -> Eff es a
runUserRepoPure usersList = reinterpret (evalState usersList) $ \env -> \case
  GetUsers ->
    gets id
  AddUser user -> do
    modify (\users -> user : users)
    gets id


-------------
-- Example --
-------------

testUsers :: UserRepo :> es => Eff es [User]
testUsers = do
  _ <- addUser (User "name" "email" "password")
  getUsers

runTest :: [User]
runTest = runPureEff . runUserRepoPure [] $ testUsers
