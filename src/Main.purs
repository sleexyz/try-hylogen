module Main where

import Prelude
import App.Layout as A
import App.Routes (match)
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Pux (App, Config, CoreEffects, fromSimple, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Router (sampleUrl)
import Signal ((~>))
import TryHylogen



-- | App configuration
config :: forall eff. A.State -> Eff (AppEffectsWith eff) (Config A.State A.Action AppEffects)
config state = do
  urlSignal <- sampleUrl
  let routeSignal = urlSignal ~> \r -> A.PageView (match r)
  pure
    { initialState: state
    , update: A.update
    , view: A.view
    , inputs: [routeSignal]
    }

-- | Entry point for the browser.
main :: A.State -> Eff (CoreEffects AppEffects) (App A.State A.Action)
main state = do
  app <- start =<< config state
  renderToDOM "#app" app.html
  pure app

-- | Entry point for the browser with pux-devtool injected.
debug :: A.State -> Eff (CoreEffects AppEffects) (App A.State (Pux.Devtool.Action A.Action))
debug state = do
  app <- Pux.Devtool.start =<< config state
  renderToDOM "#app" app.html
  pure app
