{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Services.Users
  ( getUsers
  , addUser
  , runUserRepoPure
  , runUserRepoMVar
  , UserRepo
  , User (..)
  ) where

import Control.Concurrent

import Effectful
import Effectful.Dispatch.Dynamic (reinterpret, send)
import Effectful.State.Static.Shared (evalState, evalStateMVar, gets, modify)

-----------
-- Model --
-----------

data User = User
  {name :: String, email :: String, password :: String}
  deriving (Show, Eq)

-----------------------
-- Effect definition --
-----------------------

data UserRepo :: Effect where
  GetUsers :: UserRepo m [User]
  AddUser :: User -> UserRepo m [User]

type instance DispatchOf UserRepo = 'Dynamic

---------
-- API --
---------

getUsers :: (UserRepo :> es) => Eff es [User]
getUsers = send GetUsers

addUser :: (UserRepo :> es) => User -> Eff es [User]
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

runUserRepoMVar :: MVar [User] -> Eff (UserRepo : es) a -> Eff es a
runUserRepoMVar usersList = reinterpret (evalStateMVar usersList) $ \env -> \case
  GetUsers ->
    gets id
  AddUser user -> do
    modify (\users -> user : users)
    gets id

-------------
-- Example --
-------------

testUsers :: (UserRepo :> es) => Eff es [User]
testUsers = do
  _ <- addUser (User "name" "email" "password")
  getUsers

runTest :: [User]
runTest = runPureEff . runUserRepoPure [] $ testUsers
