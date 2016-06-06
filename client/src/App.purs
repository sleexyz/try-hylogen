module App.App where

import Prelude hiding (div)
import Pux.Html hiding (bind)
import Pux.Html.Events as E
import Pux.Html.Attributes as A
import Pux

import Data.Argonaut
import Data.Either (Either (Right, Left), either)
import Network.HTTP.Affjax (AJAX, post)
import Control.Monad.Aff (Aff, attempt)

import App.Input as Input

type State = { input :: Input.State
             , output :: Response
             , sent :: Boolean
             }

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

data Action = Submit
            | InputAction Input.Action
            | Receive (Either String Response)

init :: State
init = { input: Input.init
       , output: Code "hello"
       , sent: false
       }


onSubmit :: forall eff. State -> Aff (ajax :: AJAX | eff) Action
onSubmit state = do
  res <- attempt $ post "http://localhost:8080/compile" (fromString state.input.source)
  let decode r = decodeJson r.response :: Either String Response
  let response = either (Left <<< show) decode res
  return $ Receive response

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (InputAction action) state = noEffects $ state {input= Input.update action state.input}
update (Submit) state = { state: state , effects: [onSubmit state]}
update (Receive (Left strerr)) state = noEffects $ state { output = Err strerr }
update (Receive (Right err)) state = noEffects $ state { output = err }

-- TODO: debounce
view :: State -> Html Action
view state =
  div
    [A.className "app"]
    [ h1 [] [ text "Try Hylogen"]
    , div
      [A.className "content"]
      [ div
        []
        [ InputAction <$> Input.view state.input
        , div [] [button [ E.onClick (const Submit) ] [ text "Submit" ]]
        ]
      , pre [] [ text (showOutput state.output)]
      ]
    ]
