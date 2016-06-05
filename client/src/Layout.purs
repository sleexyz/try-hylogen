module App.Layout where

import App.Input as Input
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)
import Pux
import Network.HTTP.Affjax (AJAX, post)
import Control.Monad.Aff (attempt)

data Action
  = Child (Input.Action)
  | PageView Route

type State =
  { route :: Route
  , count :: Input.State }

init :: State
init =
  { route: NotFound
  , count: Input.init }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (PageView route) state = noEffects $ state { route = route }
-- update (Child action) state = noEffects $ state { count = Input.update action state.count }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home -> map Child $ Input.view state.count
        NotFound -> App.NotFound.view state
    ]
