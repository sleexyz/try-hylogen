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


type State = { localSource :: String}

init :: State
init = { localSource: "let color = vec4 (1, 1, 1, 1)\nin toProgram color" }


data Action = UpdateSource String
            -- | SendSource String

-- update :: forall eff. Action -> State -> EffModel State Action eff
update (UpdateSource str) state = noEffects $ state {localSource=str}
-- update (UpdateSource str) state = { state: state {lastSent=100}
--                                   , effects:  [later' $ return $ SendSource (state.localSource)]
--                                   }


view :: State -> Html Action
view state = textarea [ E.onChange (\e -> UpdateSource e.currentTarget.value)
                      , A.className "input"
                      ] [ text (state.localSource) ]
