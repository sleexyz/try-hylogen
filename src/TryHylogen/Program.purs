module TryHylogen.Program where

import Pux.Html (Html, Attribute)

foreign import fromReact :: forall a.
                            Array (Attribute a) ->
                            Array (Html a) ->
                            Html a
