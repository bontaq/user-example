{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (div)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSocket
import qualified Network.Wai.Handler.WebSockets as WaiWebSocket

import Effectful

import Purview
import Server (webSocketHandler, httpHandler)

import Services.Users

import qualified Pages.Home as Home

type Path = String

root :: UserRepo :> es => Path -> Purview () (Eff es)
root location = case location of
  "/"            -> Home.render
  "/create-user" -> undefined
  _ -> div [ text "Unknown test" ]

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
