module TryHylogen where

import DOM (DOM)
import Network.HTTP.Affjax (AJAX)

type AppEffectsWith eff =  (dom :: DOM, ajax :: AJAX | eff)
type AppEffects = AppEffectsWith ()

