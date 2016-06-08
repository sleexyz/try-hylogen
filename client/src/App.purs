module App.App where

import Prelude hiding (div)
import Pux.Html hiding (bind)
import Pux.Html.Events as H
import Pux.Html.Attributes as H
import Pux

import Data.Argonaut
import Data.Either (Either (Right, Left), either)
import Network.HTTP.Affjax (AJAX, post)
import Control.Monad.Aff (Aff, attempt, later, later')

import Data.Time as D
import Data.Date as D
import Data.Int hiding (fromString)




data Response = Err String
              | Code String

showOutput :: Response -> String
showOutput (Err str) = str
showOutput (Code str) = str

instance decodeJsonResponse :: DecodeJson Response where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    contents <- obj .? "contents"
    case tag of
      _ | tag == "Code" -> return $ Code contents
        | tag == "Err"  -> return $ Err contents
        | otherwise     -> return $ Err "unexpected error"



type State = { input :: String
             , output :: Response
             , counter :: Int
             }

init :: State
init = { input: ts
       , output: Code "Type something!"
       , counter: 0
       }
  where
    ts = "color = vec4 (1, 1, 1, 1)\n\noutput = toProgram color"

data Action = Submit
            | UpdateSource String
            | Receive (Either String Response)




update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (UpdateSource str) state    = { state: state {input=str} , effects: []}
update (Submit) state              = {state: state, effects: [onSubmit state]}
  where
    onSubmit :: forall eff. State -> Aff (ajax :: AJAX | eff) Action
    onSubmit state = do
      res <- attempt $ post "http://localhost:8080/compile" (fromString state.input)
      let decode r = decodeJson r.response :: Either String Response
      let response = either (Left <<< show) decode res
      return $ Receive response
update (Receive (Left err)) state  = noEffects $ state { output = Err err}
update (Receive (Right str)) state = noEffects $ state { output = str }


-- TODO: debounce
view :: State -> Html Action
view state =
  div
    [H.className "app"]
    [ h1 [] [ text "Try Hylogen"]
    , div
      [H.className "content"]
      [ div
        []
        [ textarea [ H.onChange (\e -> UpdateSource e.currentTarget.value)
                   , H.className "input"
                   ] [ text (state.input) ]
        , div [] [button [ H.onClick (const Submit) ] [ text "Submit" ]]
        ]
      , pre [] [ text (showOutput state.output)]
      ]
    ]
