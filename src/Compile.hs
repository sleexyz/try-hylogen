{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile where

import qualified Language.Haskell.Interpreter as I
import Data.Aeson
import Hylogen.WithHylide

data Msg = Err String
         | Code String

instance ToJSON Msg where
  toJSON = \case
    Err str -> object [ "error" .= str ]
    Code str -> object [ "code" .= str ]

interp :: String -> I.InterpreterT IO String
interp str = do
  I.set [ I.languageExtensions I.:= [I.DataKinds, I.GADTs]]
  I.setImports ["Prelude", "Hylogen.WithHylide", "Hylogen.Expr", "Hylogen.Program"]
  show <$> I.interpret str (I.as :: Program)


say = I.liftIO . putStrLn

compile :: String -> IO Msg
compile str = I.runInterpreter (interp str) >>= return . \case
  Left err -> case err of
    I.UnknownError str -> Err str
    I.WontCompile errors -> Err . mconcat $ I.errMsg <$> errors
    I.NotAllowed str -> Err str
    I.GhcException str -> Err str
  Right str -> Code str

