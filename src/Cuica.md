In this article, I'm going to teach you how to build the instrument you see above on klank.dev.

tl;dr. Click [here] from mobile Firefox to play the instrument and [here] to see the code.

[klank.dev](https://klank.dev) is a PureScript sandbox for interactive animations and audio. It uses a technique called Functional Reactive Programming to blend together realtime input like touch events and the microphone.

This article proceeds in small steps, adding new parts of the instrument in each example. Links to full-working examples are provided along the way.

## Before you begin

While we'll be using klank.dev as our editor, if you'd like to experiment with the code, I'd recommend using an industrial editor like vim or VSCode. In VSCode (which I use), you can download the [purescript-ide-extension](). Also, you'll need to install `purescript` and `spago`, which can be done like so:

```bash
npm install -g purescript spago
```

If you're just following along and making minor tweaks, on the other hand, developing directly in klank.dev is fine.

## Starting with a loop

We'll start with a looped sound, which will give us the opportunity to explore how a klank is organized from top to bottom.

All klanks have a `scene` - in this case, our loop - that is passed to either `runInBrowser` for pure scenes and `runInBrowser_` for scenes with effectful input like a mouse, MIDI, or touch.

Our scene is a function of time, but for now we do not use the time parameter. We just loop a sound. `1.0` corresponds to the playback rate, `0.0` to the start position in the file in seconds, and `0.0` to the end position _or_ the full file length if equal to `0.0` (which is the case here).

```haskell
scene time = pure (speaker' $ loopBuf "cuica" 1.0 0.0 0.0)
```

The buffer `"cuica"` is downloaded using `makeBuffersKeepingCache` in the main function, which caches files in the current session for future use. Without this, the file would be downloaded every time you press play.

> Cuica is actually my friend [Ryan Veillet]() doing an improvisation with his voice! But it sounds like a cuica to me, so I call it cuica :)

The full example is below, and you can listen to it [here]().

```haskell

```

## Adding two oscillators

Let's add two periodic oscillators. They'll be pretty boring and static for now, but we'll spice them up later on.

[Periodic oscillators]() are defined using vectors that describe the coefficients of real and imaginary parts of an oscillator's harmonics. The real part effects the amplitude or loudness, whereas the imaginary part effects the phase. Harmonics with different phase relationships have create surprisingly different results!

In this example, I define one periodic oscillator called `"smooth"` and use it in the scene with a small volume adjustment. You can listen [here]().

```haskell

```

## Using the loop to modulate the oscillators

Let's now use the vocal loop to control the gain of the oscillators. This can be done by multiplying the oscillators by the amplitude, or loudness, of the loop.

To get the loudness of the loop, we need to create a custom [audio worklet](). Audio worklets are JavaScript or WebAssembly classes that process sound in 128-frame chunks. Because they need to operate at the audio rate, they should have as little processing as possible. In my worklet, I take the input sound and average together the absolute value of all 128 frames. This is a good proxy for the sound's amplitude.

The full JS amplitude tracking worklet is below:

```javascript

```

You can use klank.dev to upload a worklet to the cloud, or you can stash it wherever you'd like. Then, from your session, you can use it in like this:

```haskell

```

Now that we have our worklet imported, we can use it to measure the amplitude of the cuica loop and modulate the amplitude of the oscillators with the output. Because we have several oscillators, we use the `dup2` function, which reuses an arbitrary input (in this case, our loop buffer and our amplitude tracker) instead of instantiating a new one. Reusing audio units is a good way to keep processing time down and avoid nasty skips and pops.

The full example is below, and you can listen to it [here].

## Chaging the oscillator pitch over time

Let's now change our upper oscillator subtly over time. To my ear, this gives the cuica an odd, mystical quality.

Our scene has a `time` parameter that we haven't used yet. Now let's use it! The only modification we need to make in our example is changing the pitch of the oscillators as a function of time. We'll use a sine wave with a period of once every five seconds, which is slightly longer than the looped sound. This will ensure that the upper oscillator is always subtly different when it returns to the loop.

The full example can be heard here and is below as well:

```haskell

```

## Adding feedback

## Adding an additional recording to the mix

By now, something about this sound evokes the sound of the Balinese Gamelan. I'd like to add just a hint of gamelan bells, and I'll do that by using the same amplitude tracker to gate a loop of a balinese orchestra that I grabbed from freesound.org. The full example is below and can be heard [here].

```haskell

```

## Using touch/mouse input to trigger a tongue drum

Now it's time to make the example interactive. We'll do this by adding touch events, and as we're building the example on the computer, we'll substitute in mouse events instead of touch events for non-mobile interaction.

The full example is below and can be found [here] for mobile and [here] for desktop.

## Showing touch/mouse interaction on the canvas

Let's paint the canvas so that we have an easier time seeing where our drum's pitches fall.

The full example is below and can be found [here] for mobile and [here] for desktop.

## Adding a voice-controlled pad

The last bit we'll add is a warm pad that gets louder when the microphone input is louder. We'll set the pitch to the same pitch as the bells, and we'll sing in the same scale as the bells to create a unified harmony throughout the instrument.

To do this, we'll use the same amplitude tracking strategy as before, except this time it will be with the microphone instead of the buffer. As we sing louder, the harmonic pad will get louder. We'll also use a slightly simpler oscillator with more instances to create an overtone effect.

The final full example is below and can be found [here] for mobile and [here] for desktop.

## Conclusion

[klank.dev](https://klank.dev) is a full-featured browser-based audio-visual sandbox that allows you to build interactive artwork and applications. You can find documentation on [docs.klank.dev](https://docs.klank.dev) and plenty of examples on [discourse.klank.dev](https://discourse.klank.dev). I'm excited to see what you make with it!
