{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (div)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSocket
import qualified Network.Wai.Handler.WebSockets as WaiWebSocket
import           Server (webSocketHandler, httpHandler)

import           Effectful (runEff, type (:>), Eff, IOE)

import           Purview

import           Services.Users (UserRepo, runUserRepoPure)
import qualified Pages.Home as Home
import qualified Pages.CreateUser as CreateUser

type Path = String

{-

Top level for the website

-}
root :: (IOE :> es, UserRepo :> es) => Path -> Purview () (Eff es)
root location = case location of
  "/"            -> Home.render
  "/create-user" -> CreateUser.render
  _ -> div [ text "Unknown test" ]

{-

This controls how things are being interpreted, and can
easily be swapped around for different implmentations.

From the type signature, if you squint, you can see
it removes effects and getting everything to IO

-}
interpreter :: Eff '[UserRepo, IOE] a -> IO a
interpreter = runEff . runUserRepoPure []

main :: IO ()
main =
  let
    port = 8001
    settings = Warp.setPort port Warp.defaultSettings
  in
   Warp.runSettings settings
     $ WaiWebSocket.websocketsOr
         WebSocket.defaultConnectionOptions
         (webSocketHandler Main.interpreter root)
         (httpHandler Main.interpreter root)
