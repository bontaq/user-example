{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (div)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSocket
import qualified Network.Wai.Handler.WebSockets as WaiWebSocket

import Purview
import Server (webSocketHandler, httpHandler)

type Path = String

root :: Path -> Purview () IO
root location = case location of
  "/"            -> undefined
  "/create-user" -> undefined
  _ -> div [ text "Unknown test" ]

main :: IO ()
main =
  let
    port = 8001
    settings = Warp.setPort port Warp.defaultSettings
  in
   Warp.runSettings settings
     $ WaiWebSocket.websocketsOr
         WebSocket.defaultConnectionOptions
         (webSocketHandler root)
         (httpHandler root)
