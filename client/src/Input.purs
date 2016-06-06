module App.Input where

import Prelude hiding (div)
import Pux.Html hiding (bind)
import Pux.Html.Events as E
import Pux.Html.Attributes as A
import Pux

import Data.Argonaut
import Data.Either (Either (Right, Left), either)
import Network.HTTP.Affjax (AJAX, post)
import Control.Monad.Aff (Aff, attempt)


type State = {source :: String}

init :: State
init = { source: "let color = vec4 (1, 1,1, 1)\nin toProgram color" }


data Action = UpdateSource String

update :: Action -> State -> State
update (UpdateSource str) state = state {source=str}
-- Now how do we extend the effects?


view :: State -> Html Action
view state = textarea [ E.onChange (\e -> UpdateSource e.currentTarget.value)
                      , A.className "input"
                      ] [ text (state.source) ]
