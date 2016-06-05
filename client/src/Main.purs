module Main where

import App.Input
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Prelude (bind, return)
import Control.Monad.Eff.Console (log, CONSOLE)
import Pux (App, Config, CoreEffects, fromSimple, renderToDOM)
import Network.HTTP.Affjax (AJAX)

type AppEffects = (dom :: DOM, console :: CONSOLE, ajax :: AJAX)

-- | App configuration
-- config :: forall eff. State -> Eff (AppEffects) (Config State Action AppEffects)
config state = return  { initialState: state
                       , update: update
                       , view: view
                       , inputs: []
                       }

-- main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  app <- Pux.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  return app

-- debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
  app <- Pux.Devtool.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  return app
