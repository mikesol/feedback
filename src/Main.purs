module Main where

import Prelude

import Control.Apply.Indexed ((:*>))
import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Foldable (for_)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Math (cos, pi, sin)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit (TBandpass, TDelay, TGain, THighpass, TMicrophone, TSpeaker)
import WAGS.Graph.Parameter (AudioParameter, ff)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, defaultFFIAudio, getMicrophoneAndCamera, makeUnitCache)
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, SceneI, RunEngine, run)

vol = 1.4 :: Number

ap' :: Number -> AudioParameter
ap' = ff 0.06 <<< pure

type SceneType
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ { gain0 :: Unit, gain1 :: Unit, gain2 :: Unit }
    -- feedback0
    , gain0 :: TGain /\ { microphone :: Unit, hpf0 :: Unit, delay_1_2 :: Unit }
    , hpf0 :: THighpass /\ { delay0 :: Unit }
    , delay0 :: TDelay /\ { atten0 :: Unit }
    , atten0 :: TGain /\ { gain0 :: Unit }
    -- feedback1
    , gain1 :: TGain /\ { microphone :: Unit, bpf1 :: Unit }
    , bpf1 :: TBandpass /\ { delay1 :: Unit }
    , delay1 :: TDelay /\ { atten1 :: Unit }
    , atten1 :: TGain /\ { gain1 :: Unit, delayX :: Unit }
    -- feedback2
    , gain2 :: TGain /\ { microphone :: Unit, hpf2 :: Unit }
    , hpf2 :: THighpass /\ { delay2 :: Unit }
    , delay2 :: TDelay /\ { atten2 :: Unit }
    , atten2 :: TGain /\ { gain2 :: Unit }
    -- intermediary feedback
    , delay_1_2 :: TDelay /\ { gain_1_2 :: Unit }
    , gain_1_2 :: TGain /\ { gain2 :: Unit, gain1 :: Unit }
    -- full loop
    , delayX :: TDelay /\ { mix :: Unit }
    -- microphone
    , microphone :: TMicrophone /\ {}
    }

type Env
  = SceneI Unit Unit

type FrameTp p i o a
  = IxWAG RunAudio RunEngine p Unit i o a

doChanges :: forall proof. Env -> FrameTp proof SceneType SceneType Unit
doChanges { time } =
  ivoid
    $ ichange
        { hpf0: ap' $ sin (time * pi * 0.5) * 1000.0 + 1500.0
        , delay0: ap' (0.4 + sin (time * pi * 2.0) * 0.2)
        , bpf1: ap' $ cos (time * pi * 1.6) * 1000.0 + 1500.0
        , delay1: ap' (0.3 + cos (time * pi * 0.7) * 0.1)
        , hpf2: ap' $ cos (time * pi * 4.0) * 1000.0 + 1500.0
        , delay2: ap' (0.1 + sin (time * pi * 0.2) * 0.07)
        }

createFrame :: Env -> FrameTp Frame0 {} SceneType Unit
createFrame env =
  ipatch
    :*> ichange
        { atten0: 0.6
        , gain0: 0.5
        , atten1: 0.6
        , gain1: 0.5
        , atten2: 0.6
        , gain2: 0.5
        , gain_1_2: 0.7
        , delay_1_2: 0.9
        , mix: 1.0
        }
    :*> doChanges env

piece :: Scene Env RunAudio RunEngine Frame0 Unit
piece =
  createFrame
    @!> iloop (const <<< doChanges)

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI component unit body

type State
  = { unsubscribe :: Effect Unit
    , audioCtx :: Maybe AudioContext
    }

data Action
  = StartAudio
  | StopAudio

component :: forall query input output m. MonadEffect m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  }

classes :: forall r p. Array String -> HP.IProp ( class :: String | r ) p
classes = HP.classes <<< map ClassName

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ classes [ "w-screen", "h-screen" ] ]
    [ HH.div [ classes [ "flex", "flex-col", "w-full", "h-full" ] ]
        [ HH.div [ classes [ "flex-grow" ] ] []
        , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
            [ HH.div [ classes [ "flex-grow" ] ] []
            , HH.div_
                [ HH.h1 [ classes [ "text-center", "text-3xl", "font-bold" ] ]
                    [ HH.text "Fun with feedback" ]
                , HH.p [ classes [ "text-center" ] ]
                    [ HH.text "Use headphones!" ]
                , HH.button
                    [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> StartAudio ]
                    [ HH.text "Start audio" ]
                , HH.button
                    [ classes [ "text-2xl", "m-5", "bg-pink-500", "p-3", "rounded-lg", "text-white", "hover:bg-pink-400" ], HE.onClick \_ -> StopAudio ]
                    [ HH.text "Stop audio" ]
                ]
            , HH.div [ classes [ "flex-grow" ] ] []
            ]
        , HH.div [ classes [ "flex-grow" ] ] []
        ]
    ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    { microphone } <- H.liftAff $ getMicrophoneAndCamera true false
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    let
      ffiAudio =
        (defaultFFIAudio audioCtx unitCache)
          { microphone = toNullable microphone
          }
    unsubscribe <-
      H.liftEffect
        $ subscribe
            (run (pure unit) (pure unit) { easingAlgorithm } (FFIAudio ffiAudio) piece)
            (const $ pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
