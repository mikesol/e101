module E101.Cuica where

import Prelude
import Color (rgb)
import Data.Array (catMaybes, head, index, length, mapWithIndex, range, take)
import Data.Foldable (fold, foldl)
import Data.Foldable (traverse_)
import Data.Int (floor, toNumber)
import Data.List (List(..), fold, (:))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
import FRP.Behavior.Audio (AV(..), AudioParameter, AudioUnit, CanvasInfo(..), EngineInfo, IAudioUnit(..), audioWorkletProcessor, constant_, convolver_, defaultExporter, dup2, evalPiecewise, g'add_, g'delay_, g'gain_, gainT_, gainT_', gain_, gain_', graph, graph_, highpass_, loopBuf, loopBuf_, makePeriodicWave, microphone, pannerMono, pannerMonoT_, pannerMono_, panner_, periodicOsc, periodicOsc_, playBufWithOffset_, playBuf_, runInBrowser, runInBrowser_, speaker, speaker')
import FRP.Behavior.Audio (AudioParameter, AudioUnit, convolver_, dup2, evalPiecewise, g'add, g'delay, g'gain, gainT_', gain_', graph, playBufWithOffset_, runInBrowser, speaker')
import FRP.Behavior.Mouse (buttons)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Mouse (Mouse, getMouse)
import Foreign.Object as O
import Graphics.Drawing (fillColor, filled, rectangle)
import Math (cos, pi, pow, sin, (%))
import Math (pi, pow, sin)
import Record.Extra (SLProxy(..), SNil)
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))
import Type.Data.Graph (type (:/))
import Type.Klank.Dev (Klank, Klank', defaultEngineInfo, klank, makeBuffersKeepingCache)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Web.TouchEvent.Touch as T
import Web.TouchEvent.TouchEvent (TouchEvent, changedTouches, fromEvent, touches)
import Web.TouchEvent.TouchList as TL
import Web.UIEvent.MouseEvent (MouseEvent, clientX)
import Web.UIEvent.MouseEvent as ME

conv440 :: Number -> Number
conv440 i = 440.0 * (2.0 `pow` (i / 12.0))

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

boundPlayer :: forall a. Number -> (Number -> List a) -> Number -> List a
boundPlayer len a time = if (time) + kr >= 0.0 && time < (len) then a time else Nil

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

type TouchOnset
  = Array
      { id :: Int
      , x :: Number
      , y :: Number
      }

type TouchAccumulatorSingleton
  = { id :: Int
    , x :: Number
    , y :: Number
    , t :: Number
    }

type TouchAccumulator
  = Array TouchAccumulatorSingleton

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

type PWF
  = Array (Tuple Number Number)

ots :: Array Number
ots = map (2.0 * _) [ 0.0, 0.125, 0.1875, 0.3125, 0.5 ]

kos :: Int -> M.Map Int TouchAccumulatorSingleton -> Number -> Int
kos i m n = maybe 0 (\v -> floor $ 20.0 * (min 1.0 (n - v.t))) (M.lookup i m)

scene :: Touch -> TouchAccumulator -> CanvasInfo -> Number -> Behavior (AV D2 TouchAccumulator)
scene touch acc (CanvasInfo ci) time = f <$> nac
  where
  nac :: Behavior TouchAccumulator
  nac =
    ( \t ->
        if map _.id (head acc) == map _.id (head t) then
          take 15 acc
        else
          ((maybe [] (\{ id, x, y } -> [ { id, x, y, t: time } ]) (head t)) <> acc)
    )
      <$> (touching touch)

  f newAcc =
    let
      keys = foldl (\m v -> M.insertWith (\v0 v1 -> if v0.t > v1.t then v0 else v1) (floor (5.0 * v.y / ci.h)) v m) M.empty newAcc
    in
      ( AV
          ( Just
              ( dup2 (loopBuf "cuica" 1.0 0.0 0.0) \d ->
                  speaker
                    ( d
                        :| ( ( fold
                                $ map (\func -> func time)
                                    ( map
                                        ( \{ id, x, y, t } ->
                                            atT t
                                              $ ( boundPlayer 3.0 \tm ->
                                                    pure
                                                      $ playBuf_ ("buf" <> show id)
                                                          "tongue"
                                                          (2.0 + fromMaybe 1.5 (ots `index` (floor (5.0 * y / ci.h))))
                                                )
                                        )
                                        newAcc
                                    )
                            )
                              <> ( graph
                                    { aggregators:
                                        { out: Tuple (g'add_ "gout") (SLProxy :: SLProxy ("combine" :/ SNil))
                                        , combine: Tuple (g'add_ "ga") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                                        , gain: Tuple (g'gain_ "gg" 0.3) (SLProxy :: SLProxy ("del" :/ SNil))
                                        }
                                    , processors:
                                        { del: Tuple (g'delay_ "gd" 0.2) (SProxy :: SProxy "combine")
                                        }
                                    , generators:
                                        { mic:
                                            ( ( gain_ "cuicaGain" 1.0
                                                  ( pannerMono 0.0 (periodicOsc "smooth" (900.0 + 200.0 * sin (time * pi * 0.2)))
                                                      :| (gain_' "bli" 2.0 (loopBuf_ "bali" "bali" 1.0 0.0 0.0))
                                                      : Nil
                                                  )
                                              )
                                                * audioWorkletProcessor
                                                    "klank-amplitude"
                                                    O.empty
                                                    d
                                            )
                                        }
                                    }
                                )
                              : Nil
                          )
                    )
              )
          )
          ( Just
              ( fold
                  ( map
                      ( \i ->
                          filled
                            ( fillColor case i of
                                0 -> (rgb 23 (67 + kos i keys time) 189)
                                1 -> (rgb (89 + kos i keys time) 67 89)
                                2 -> (rgb 23 167 (29 + kos i keys time))
                                3 -> (rgb (200 + kos i keys time) 35 65)
                                4 -> (rgb 203 (210 + kos i keys time) 190)
                                _ -> (rgb 23 67 189)
                            )
                            ( rectangle 0.0 ((ci.h * toNumber i) / 5.0) ci.w (ci.h / 5.0)
                            )
                      )
                      (range 0 4)
                  )
              )
          )
          newAcc
      )

main :: Klank' (TouchAccumulator)
main =
  klank
    { run =
      runInBrowser_ do
        touch <- getTouch
        pure (scene touch)
    , accumulator = \res _ -> res []
    , worklets =
      \_ res rej ->
        res
          [ "https://klank-share.s3.eu-west-1.amazonaws.com/K16050057737584320.js"
          ]
    , buffers =
      makeBuffersKeepingCache
        [ Tuple "cuica" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/ryanCuica.ogg"
        , Tuple "bali" "https://freesound.org/data/previews/257/257625_3932570-lq.mp3"
        , Tuple "tongue" "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/TongueDrum/Mallet-A2_1.ogg"
        ]
    , periodicWaves =
      \ctx _ res rej -> do
        pw <-
          makePeriodicWave ctx
            (0.5 +> 0.25 +> -0.1 +> 0.07 +> 0.1 +> empty)
            (0.2 +> 0.1 +> 0.01 +> -0.03 +> -0.1 +> empty)
        res $ O.singleton "smooth" pw
    , exporter = defaultExporter
    }

newtype Touch
  = Touch
  { touches :: Ref.Ref (TouchOnset)
  , dispose :: Effect Unit
  }

handleTE :: Int -> Ref.Ref (TouchOnset) -> TouchEvent -> Effect Unit
handleTE i ref te = do
  let
    ts = changedTouches te
  let
    l = TL.length ts
  let
    tlist = map (\t -> { id: i, x: toNumber $ T.clientX t, y: toNumber $ T.clientY t }) (catMaybes $ map (\x -> TL.item x ts) (range 0 (l - 1)))
  void $ Ref.modify (\ipt -> tlist <> ipt) ref

handleM :: Int -> Ref.Ref (TouchOnset) -> MouseEvent -> Effect Unit
handleM i ref me = do
  let
    x = clientX me
  let
    y = clientX me
  void $ Ref.modify (\ipt -> [ { id: i, x: toNumber x, y: toNumber y } ] <> ipt) ref

-- | Get a handle for working with the mouse.
getTouch :: Effect Touch
getTouch = do
  nTouches <- Ref.new 0
  touches <- Ref.new []
  target <- toEventTarget <$> window
  touchStartListener <-
    eventListener \e -> do
      fromEvent e
        # traverse_ \me -> do
            nt <- Ref.modify (_ + 1) nTouches
            handleTE nt touches me
  mouseDownListener <-
    eventListener \e -> do
      ME.fromEvent e
        # traverse_ \me -> do
            nt <- Ref.modify (_ + 1) nTouches
            handleM nt touches me
  addEventListener (wrap "touchstart") touchStartListener false target
  addEventListener (wrap "mousedown") mouseDownListener false target
  let
    dispose = do
      removeEventListener (wrap "touchstart") touchStartListener false target
      removeEventListener (wrap "mousedown") mouseDownListener false target
  pure (Touch { touches, dispose })

-- | Create an event which also returns the current mouse buttons.
withTouch ::
  forall a.
  Touch ->
  Event a ->
  Event { value :: a, touches :: TouchOnset }
withTouch (Touch { touches }) e =
  makeEvent \k ->
    e
      `subscribe`
        \value -> do
          touchValue <- Ref.read touches
          k { value, touches: touchValue }

-- | A `Behavior` which reports the number of touches.
touching :: Touch -> Behavior (TouchOnset)
touching m = behavior \e -> map (\{ value, touches: bs } -> value bs) (withTouch m e)
