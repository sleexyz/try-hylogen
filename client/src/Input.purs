module App.Input where

import Prelude
import Pux.Html (Html, h1, div, span, button, text)
import Pux.Html.Events (onClick)
import Pux

import Data.Argonaut
import Data.Generic
import Data.Either
import Network.HTTP.Affjax (AJAX, post)
import Control.Monad.Aff (attempt)


type State = {input :: String, output :: Response}

-- data ResponseType = Err | Code

data Response = Err String
              | Code String
showOutput :: Response -> String
showOutput (Err str) = str
showOutput (Code str) = str

-- instance showResponse :: Show Response where
--   show (Err str) = str
--   show (Code str) = str

instance decodeJsonResponse :: DecodeJson Response where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    contents <- obj .? "contents"
    case tag of
      _ | tag == "Code" -> return $ Code contents
        | tag == "Err"  -> return $ Err contents
        | otherwise     -> return $ Err "unexpected error"

data Action = RequestCode String
            | ReceiveCode (Either String Response)

init :: State
init = {input: "hadfasfd", output : Code "hello"}

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (RequestCode str) s = { state: (s {input = str})
                             , effects: [ do
                                             res <- attempt $ post "http://localhost:8080/compile" "hello"
                                             let decode r = decodeJson r.response :: Either String Response
                                             let todos = either (Left <<< show) decode res
                                             return $ ReceiveCode (Left "hello")
                                        ]
                             }
update (ReceiveCode (Left strerr)) state = noEffects $ state { output = Err strerr }
update (ReceiveCode (Right err)) state = noEffects $ state { output = err }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "hello!"]
    , div [] [ text (state.input) ]
    -- , div [] [ text (showOutput state.output)]
    , button [ onClick (const $ RequestCode "poopy") ] [ text "Submit" ]
    ]
