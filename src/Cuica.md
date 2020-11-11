{% youtube sZfJluQzamM %}

In this article, I'll show you how I built the instrument you see above on klank.dev. The whole thing is just 250 lines of PureScript. You can click [here](https://link.klank.dev/6uEh7kup1XtW7q5RA) from mobile Firefox to play the instrument and [here](https://link.klank.dev/vJFCfbU3Fd75CQVe8) to run the code in klank.

[klank.dev](https://klank.dev) is a PureScript sandbox for interactive animations and audio. It uses a technique called Functional Reactive Programming, and more specifically the [Behavior](https://github.com/paf31/purescript-behaviors) pattern, to turn a phone, tablet or comptuer into a musical instrument.

This article explores small snippets from the larger klank, showing how each one adds up to the full instrument.

## Before you begin

While we'll be using klank.dev as our editor, if you'd like to experiment with the code, I'd recommend using an industrial editor like vim or VSCode. In VSCode (which I use), you can download the [`vscode-ide-purescript` Extension](https://github.com/nwolverson/vscode-ide-purescript). Also, you'll need to install `purescript` and `spago`, which can be done like so:

```bash
npm install -g purescript spago
```

If you're just following along and making minor tweaks, on the other hand, developing directly in klank.dev is fine.

## The main cuica loop

The main cuica loop is on line 95: `(loopBuf "cuica" 1.0 0.0 0.0)`. It is duplicated using the `dup2` function so that the same loop can be fed to multiple parts of the audio graph, which speeds up computation.

The buffer `"cuica"`, along with all of the other buffers, are downloaded using `makeBuffersKeepingCache`. This caches files in the current session for future use. Without this, the file would be downloaded every time you press play.

```haskell
main :: Klank' (TouchAccumulator)
main =
  klank
    {
    -- other stuff
    buffers =
      makeBuffersKeepingCache
        [ Tuple "cuica" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/ryanCuica.ogg"
        , Tuple "bali" "https://freesound.org/data/previews/257/257625_3932570-lq.mp3"
        , Tuple "tongue" "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/TongueDrum/Mallet-A2_1.ogg"
        ]
    }
```

> Cuica is actually my friend [Ryan Veillet](https://www.facebook.com/ryan.veillet) doing an improvisation with his voice! But it sounds like a cuica to me, so I call it cuica :)

To listen to the original "cuica" on klank.dev, you can check out [this link](https://link.klank.dev/56aVzqSdqShH4c2e8).

## Adding an accompaniment

In the klank, we accompany the cuica sound with periodic oscillator whose volume is modulated by the input volume of the cuica. We also use Balinese bells in a loop to add depth and richness to the sound.

```haskell
( gain_ "cuicaGain" 1.0
    ( pannerMono 0.0 (periodicOsc "smooth" (900.0 + 200.0 * sin (time * pi * 0.2)))
        :| (gain_' "bli" 2.0 (loopBuf_ "bali" "bali" 1.0 0.0 0.0))
        : Nil
    )
)
* audioWorkletProcessor_ "wp-cuica"
    "klank-amplitude"
    O.empty
    d
```

The multiplication operation between the audio worklet and the oscillator multiplies the two singlas together, which in this case multiplies the accompaniment by the amplitude of the voice.

The `audioWorkletProcessor_` uses a custom audio worklet for amplitude tracking. The full worklet is quite short, and just averages together the absolute value of 128 audio frames, which at a sample rate of 44100 Hz is a good proxy for amplitude.

```javascript
// amplitude.js
class AmplitudeProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
    this.prev = new Array(10).fill(0.0);
  }
  process(inputs, outputs) {
    const input = inputs[0];
    const output = outputs[0];
    for (var j = 0; j < Math.min(input.length, output.length); j++) {
      var ichannel = input[j];
      var ochannel = output[j];
      var amp = 0.0;
      for (var i = 0; i < ichannel.length; i++) {
        amp += Math.abs(ichannel[i]);
      }
      amp /= ichannel.length ? ichannel.length : 1.0;
      for (var i = 0; i < ochannel.length; i++) {
        ochannel[i] = (this.prev[j] * (127 - i) + amp * i) / 127.0;
      }
      this.prev[j] = amp;
    }
    return true;
  }
}

registerProcessor("klank-amplitude", AmplitudeProcessor);
```

## Creating a slight echo

The echo effect you hear comes from the graph unit, which is used to create a feedback loop.

```haskell
graph
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
                * audioWorkletProcessor_ "wp-cuica"
                    "klank-amplitude"
                    O.empty
                    d
            )
        }
    }
```

Here, `mic` is fed to `combine`, which is then fed to the delay line `del`, which goes to a gain less than `1.0` and then `combine` again. The result is the decaying echo you hear.

## Using touch/mouse input to trigger a tongue drum

The code below turns discrete touch events into a behavior. The touch events are first transformed into an event, which is then turned into a `Behavior` using the `behavior` function. This is incorporated into the audio scene on line 77.

```haskell
type TouchOnset
  = Array
      { id :: Int
      , x :: Number
      , y :: Number
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
  addEventListener (wrap "touchstart") touchStartListener false target
  let
    dispose = do
      removeEventListener (wrap "touchstart") touchStartListener false target
  pure (Touch { touches, dispose })

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

touching :: Touch -> Behavior (TouchOnset)
touching m = behavior \e -> map (\{ value, touches: bs } -> value bs) (withTouch m e)
```

## Showing touch/mouse interaction on the canvas

Let's paint the canvas so that we have an easier time seeing where our drum's pitches fall.

```haskell
kos :: Int -> M.Map Int TouchAccumulatorSingleton -> Number -> Int
kos i m n = maybe 0 (\v -> floor $ 20.0 * (min 1.0 (n - v.t))) (M.lookup i m)

---

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
```

The code above colors the keyboard using the five different `rgb` values, and `kos` measures if a key is being played or not. If so, it changes the `rgb` value subtly over one second by subtracting the current time from the onset time (`n - v.t`).

## Conclusion

[klank.dev](https://klank.dev) is a full-featured browser-based audio-visual sandbox that allows you to build interactive artwork and applications. You can find documentation on [docs.klank.dev](https://docs.klank.dev) and plenty of examples on [discourse.klank.dev](https://discourse.klank.dev). I'm excited to see what you make with it!
