module App.Input where

import Prelude hiding (div)
import Pux.Html hiding (bind)
import Pux.Html.Events as E
import Pux.Html.Attributes as A
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

data Action = RequestCode
            | UpdateCode String
            | ReceiveCode (Either String Response)

init :: State
init = {input: "let color = vec4 (1, 1, 1, 0)\nin toProgram color", output: Code "hello"}

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (UpdateCode str) state = noEffects $ state { input = str }
update (RequestCode) state = { state: state
                             , effects: [ do
                                             res <- attempt $ post "http://localhost:8080/compile" (fromString state.input)
                                             let decode r = decodeJson r.response :: Either String Response
                                             let response = either (Left <<< show) decode res
                                             return $ ReceiveCode response
                                        ]
                             }
update (ReceiveCode (Left strerr)) state = noEffects $ state { output = Err strerr }
update (ReceiveCode (Right err)) state = noEffects $ state { output = err }

textboxOnChange e = UpdateCode e.currentTarget.value

-- TODO: debounce

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "hello!"]
    , textarea [ E.onChange textboxOnChange
               , A.className "input"
               ] [ text (state.input) ]
    , div [] [button [ E.onClick (const RequestCode) ] [ text "Submit" ]]
    , pre [] [ text (showOutput state.output)]
    ]
