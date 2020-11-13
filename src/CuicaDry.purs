module E101.CuicaDry where

import Prelude
import Data.Array (catMaybes, length, foldl, mapWithIndex, range)
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.List (List(..), fold, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Set (isEmpty)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1, D2)
import Data.Vec ((+>), empty)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Audio (AV(..), AudioParameter, AudioUnit, CanvasInfo(..), EngineInfo, audioWorkletProcessor, constant_, convolver_, dup2, evalPiecewise, g'add_, g'delay_, g'gain_, gainT_, gainT_', gain_, gain_', graph, graph_, highpass_, loopBuf, makePeriodicWave, microphone, pannerMono, pannerMonoT_, pannerMono_, panner_, periodicOsc, periodicOsc_, playBufWithOffset_, runInBrowser, runInBrowser_, sinOsc, speaker, speaker')
import FRP.Behavior.Audio (AudioParameter, AudioUnit, convolver_, dup2, evalPiecewise, g'add, g'delay, g'gain, gainT_', gain_', graph, playBufWithOffset_, runInBrowser, speaker')
import FRP.Behavior.Mouse (buttons)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Mouse (Mouse, getMouse)
import Foreign.Object as O
import Math (cos, pi, pow, sin, (%))
import Math (pi, pow, sin)
import Record.Extra (SLProxy(..), SNil)
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))
import Type.Data.Graph (type (:/))
import Type.Klank.Dev (Klank, defaultEngineInfo, klank, makeBuffersKeepingCache)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Web.TouchEvent.Touch as T
import Web.TouchEvent.TouchEvent (TouchEvent, fromEvent, touches)
import Web.TouchEvent.TouchList as TL

conv440 :: Number -> Number
conv440 i = 440.0 * (2.0 `pow` (i / 12.0))

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

type PWF
  = Array (Tuple Number Number)

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ dup2 (loopBuf "cuica" 1.0 0.0 0.0) \d ->
        speaker
          ( d
              :| ( gain_' "cuicaGain" 1.0 (pannerMono 0.0 $ periodicOsc "smooth" (900.0 + 200.0 * sin (time * pi * 0.2)))
                    * audioWorkletProcessor
                        "klank-amplitude"
                        O.empty
                        d
                )
              : ( gain_' "cuicaGain2" 0.7 (pannerMono 0.0 $ sinOsc (50.0 + 3.0 * sin (time * pi * 0.2)))
                    * audioWorkletProcessor
                        "klank-amplitude"
                        O.empty
                        d
                )
              : Nil
          )

main :: Klank
main =
  klank
    { run =
      runInBrowser scene
    , worklets =
      \_ res rej ->
        res
          [ "https://klank-share.s3.eu-west-1.amazonaws.com/K16050057737584320.js"
          ]
    , buffers =
      makeBuffersKeepingCache
        [ Tuple "cuica" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/ryanCuica.ogg"
        ]
    , periodicWaves =
      \ctx _ res rej -> do
        pw <-
          makePeriodicWave ctx
            (0.5 +> 0.25 +> -0.1 +> 0.07 +> 0.1 +> empty)
            (0.2 +> 0.1 +> 0.01 +> -0.03 +> -0.1 +> empty)
        res $ O.singleton "smooth" pw
    }
