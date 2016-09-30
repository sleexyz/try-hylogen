{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Compile
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Aeson
import Data.Function
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant

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
    & cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}

main :: IO ()
main =  do
  threadId <- forkIO $ run 8080 app
  putStrLn "type a newline to exit"
  _ <- getLine
  killThread threadId
