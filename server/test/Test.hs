{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TypeOperators  #-}
module Test where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Function
import           Data.Proxy
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Encoding    as T
import qualified Data.Text.Lazy.IO          as T
import           GHC.Generics
import           Network.HTTP
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Servant.API
import           Servant.Client

--  | manual HTTP request
test1 :: IO ()
test1 = do
  res <- simpleHTTP ( postRequestWithBody
                      "http://localhost:8080/compile"
                      "application/json"
                      "\"toProgram audio\""
                    )
  bod <- getResponseBody res
  print $ parseRes bod
  return ()
  where
    parseRes :: String -> Msg
    parseRes bod = (decode . T.encodeUtf8 . T.pack $ bod) & \case
      Just x -> x
      Nothing -> Err "could not parse response"






type API = "compile" :>  ReqBody '[JSON] String :> Post '[JSON] Msg

api :: Proxy API
api = Proxy

data Msg = Err String
         | Code String
         deriving (Show, Generic, ToJSON, FromJSON)

query :: Manager -> BaseUrl -> ExceptT ServantError IO Msg
query = compileReq "toProgram audio"
  where
    compileReq :: String -> Manager -> BaseUrl -> ExceptT ServantError IO Msg
    compileReq = client api


--  | derived HTTP request via servant-client
test2 :: IO ()
test2 = do
  manager <- newManager defaultManagerSettings
  res <- runExceptT (query manager (BaseUrl Http "localhost" 8080 ""))
  print res
  return ()


main :: IO ()
main = test1
