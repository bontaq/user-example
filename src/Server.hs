{-# LANGUAGE OverloadedStrings #-}
-- |

module Server
  ( webSocketHandler
  , httpHandler
  )
where

import qualified Network.Wai as Wai
import qualified Data.ByteString.Char8 as ByteString
import qualified Network.WebSockets as WebSocket
import qualified Data.Text as Text
import           Network.HTTP.Types

import Purview

webSocketHandler component pendingConnection = do
  let
    path = ByteString.unpack
      $ WebSocket.requestPath (WebSocket.pendingRequest pendingConnection)

  connection <- WebSocket.acceptRequest pendingConnection
  startWebSocketLoop defaultConfiguration { devMode=True } (component path) connection

httpHandler component request respond =
  let
    path = Text.unpack . Text.concat $ Wai.pathInfo request
  in
    respond
      $ Wai.responseBuilder
          status200
          [("Content-Type", "text/html")]
          (renderFullPage defaultConfiguration (component $ "/" <> path))