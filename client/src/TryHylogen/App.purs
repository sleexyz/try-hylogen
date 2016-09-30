module TryHylogen.App where

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

import TryHylogen.Program as Program
import TryHylogen


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
      _ | tag == "Code" -> pure $ Code contents
        | tag == "Err"  -> pure $ Err contents
        | otherwise     -> pure $ Err "unexpected error"


type State = { source :: String
             , output :: Response
             , glsl :: String
             , counter :: Int
             }

init :: State
init = { source: src
       , output: Code glsl
       , glsl: glsl
       , counter: 0
       }
  where
    src = """output :: Program
output = toProgram color

color :: Vec4
color = vec4 (0, 0, 0, 1)
"""
    glsl = """void main() {
    vec4 _1 = vec4(0.0, 0.0, 0.0, 1.0);

    gl_FragColor = _1;
}
"""


data Action = ActuallySubmit
            | UpdateSource String
            | TrySubmit Int
            | Nop
            | Receive (Either String Response)




update :: Action -> State -> EffModel State Action AppEffects
update (UpdateSource str) state
  = { state: state {source=str, counter=newCounter}
    , effects: [later' 1000 <<< pure $ TrySubmit newCounter]
    }
  where
    newCounter = state.counter + 1

update (TrySubmit counter) state =
  { state: state
  , effects: [ pure $ if counter == state.counter
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
                  pure $ Receive response
             ]
  }
update (Receive (Left err)) state
  = noEffects $ state { output = Err err}
update (Receive (Right res)) state
  = case res of
    Err str -> noEffects $ state { output = Err str}
    Code str -> noEffects $ state { output = Err str, glsl = str}
update Nop state = noEffects state


longFsSource :: String
longFsSource = """
output = toProgram rayMarch8

rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * x_ a
                   + sin phi * y_ a
                 , (-1) * sin phi * x_ a
                   + cos phi * y_ a
                 )

rep :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
rep c p = mod_  p c - 0.5 * c

box :: Vec3 -> Vec3 -> Vec3 -> Vec1
box boxPos dim p = len (max_ (abs (p - boxPos) - dim) 0)


raymarch8 :: Vec4
raymarch8 = id
  . (\x -> vec4(x, 1))
  . (\(x, _, _) -> x)
  $ foldr fn  (black, 0, true) (fromInteger <$> [1..maxSteps])
  where

    eye = vec3 (0, 0, -1)
    up = vec3 (0, 1, 0)
    right = vec3 (1, 0, 0)
    maxSteps = 32


    ro = vec3 (rot time uvN, tan (time * (-0.1)))
    rd = eye ^* 0.8 + right ^* x_ uvN + up ^* y_ uvN --perspective!
      & (\x -> vec3 (rot (time * 0.1) (vec2(x_ x, y_ x)), z_ x))
      & normalize


    sdf :: Vec3 -> Vec1
    sdf p = (box (vec3(mouse, -2.5)) 0.4 (f p))
            `min_` (box (vec3(mouse, -2.5)) (vec3(5,0.1, 0.1)) (f p))
            `min_` (box (vec3(mouse, -2.5)) (vec3(0.1,5, 0.1)) (f p))
            `min_` (box (vec3(mouse, -2.5)) (vec3(0.1,0.1, 5)) (f p))
      where
        f = rep 5


    fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
    fn i (color, t, continue) =
      let p = ro + rd ^* t
          d = sdf p

          i' = i/fromInteger maxSteps
          newColor = (mix i' (vec3(0.1, 0, 0.2)) (white ^* 0.9))

          cond = (d `lt` 0.001)
      in ( sel continue (sel cond newColor color) color
         , sel continue (sel cond t (t + d)) t
         , sel continue (sel cond false true) continue
         )



"""

-- TODO: debounce
view :: State -> Html Action
view state =
  div
    [H.className "app"]
    [ h1 [] [ text "Try Hylogen"]
    , div [H.className "hflex"]
      [ div
        [H.className "content"]
        [ textarea [ H.onChange (\e -> UpdateSource e.currentTarget.value)
                   , H.className "source"
                   ] [ text (state.source) ]
        , pre [H.className "glsl"] [ text (showOutput state.output)]
        ]
      , Program.fromReact [ H.attr "fsSource" $ state.glsl
                          , H.attr "width" 100.0
                          , H.attr "height" 100.0
                          ] []
      ]
    ]
