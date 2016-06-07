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

type State = { input :: String
             , output :: Response
             , sent :: Boolean
             }

init :: State
init = { input: "let color = vec4 (1, 1, 1, 1)\nin toProgram color"
       , output: Code "Type something!"
       , sent: false
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


data Maybe a = Some a
             | None


data List a = Nil
            | Cons a (List a)

data Action = Submit
            | UpdateSource String
            | Receive (Either String Response)



onSubmit :: forall eff. State -> Aff (ajax :: AJAX | eff) Action
onSubmit state = do
  res <- attempt $ post "http://localhost:8080/compile" (fromString state.input)
  let decode r = decodeJson r.response :: Either String Response
  let response = either (Left <<< show) decode res
  return $ Receive response

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (UpdateSource str) state = noEffects $ state {input=str}
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
        [ textarea [ E.onChange (\e -> UpdateSource e.currentTarget.value)
                   , A.className "input"
                   ] [ text (state.input) ]
        , div [] [button [ E.onClick (const Submit) ] [ text "Submit" ]]
        ]
      , pre [] [ text (showOutput state.output)]
      ]
    ]
