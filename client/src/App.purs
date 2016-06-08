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


type State = { source :: String
             , output :: Response
             , counter :: Int
             }

init :: State
init = { source: """output :: Program
output = toProgram color

color :: Vec4
color = vec4 (1, 1, 1, 1)
"""
       , output: Code """void main() {
    vec4 _1 = vec4(1.0, 1.0, 1.0, 1.0);

    gl_FragColor = _1;
}
"""
       , counter: 0
       }


data Action = ActuallySubmit
            | UpdateSource String
            | TrySubmit Int
            | Nop
            | Receive (Either String Response)




update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (UpdateSource str) state
  = { state: state {source=str, counter=newCounter}
    , effects: [later' 1000 <<< return $ TrySubmit newCounter]
    }
  where
    newCounter = state.counter + 1

update (TrySubmit counter) state =
  { state: state
  , effects: [ return $ if counter == state.counter
                        then ActuallySubmit
                        else Nop
             ]
  }

update (ActuallySubmit) state =
  { state: state
  , effects: [ do
                  res <- attempt $ post "http://localhost:8080/compile" (fromString state.source)
                  let decode r = decodeJson r.response :: Either String Response
                  let response = either (Left <<< show) decode res
                  return $ Receive response
             ]
  }
update (Receive (Left err)) state
  = noEffects $ state { output = Err err}
update (Receive (Right str)) state
  = noEffects $ state { output = str }
update Nop state = noEffects state


-- TODO: debounce
view :: State -> Html Action
view state =
  div
    [H.className "app"]
    [ h1 [] [ text "Try Hylogen"]
    , div
      [H.className "content"]
      [ textarea [ H.onChange (\e -> UpdateSource e.currentTarget.value)
                 , H.className "source"
                 ] [ text (state.source) ]
      , pre [H.className "glsl"] [ text (showOutput state.output)]
      ]
    ]
