{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}


module Main where

import Servant
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Proxy

import Control.Monad.IO.Class
import Control.Concurrent

import Compile

type API = "compile" :>  ReqBody '[JSON] String :> Post '[JSON] Msg

api :: Proxy API
api = Proxy

server :: Server API
server = compileHandle
  where
    compileHandle :: String -> Handler Msg
    compileHandle = liftIO . compile

app :: Application
app = serve api server

main :: IO ()
main =  do
  threadId <- forkIO $ run 8080 app
  putStrLn "type a newline to exit"
  _ <- getLine
  killThread threadId
