module App.Layout where

import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude
import Pux.Html (Html, div, h1, p, text)
import Pux
import TryHylogen.App as App
import DOM (DOM)

import TryHylogen

data Action
  = Child (App.Action)
  | PageView Route

type State =
  { route :: Route
  , appState :: App.State
  }

init :: State
init =
  { route: NotFound
  , appState: App.init
  }

update :: Action -> State -> EffModel State Action AppEffects
update (PageView route) state =
  { state: state { route = route }
  , effects : []
  }
update (Child action) state =
  let childEffModel = App.update action state.appState
  in { state: state {appState = childEffModel.state}
     , effects: map (map Child) childEffModel.effects
     }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home -> map Child $ App.view state.appState
        NotFound -> NotFound.view state
    ]
