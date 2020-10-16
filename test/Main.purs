module Test.Main where

import Prelude
import Data.Array (range)
import Data.Int (toNumber)
import Data.Symbol (SProxy(..))
import Data.Typelevel.Num (class Pos, D1)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, Oversample(..), makeAudioTrack, makeFloatArray, runInBrowser, speaker')
import Math (pi, abs)
import Type.Data.Row (RProxy(..))
import Type.Klank.Dev (type (:$), ConsSymbol, FloatArrays, Klank, NilS, NilSymbol, PlaySignature, SymbolListProxy, Tracks, WaveShaperSignature, Worklets, affable, affableRec, klank, tAudioWorkletGenerator, tAudioWorkletGeneratorT, tAudioWorkletProcessor, tPeriodicOsc, tPlay, tPlayBuf, tWaveShaper, toUrlArray)

makeDistortionCurve :: Number -> Array Number
makeDistortionCurve k =
  map
    ( \i ->
        let
          x = (toNumber i * 2.0 / toNumber n_samples) - 1.0
        in
          (3.0 + k) * x * 20.0 * deg / (pi + (k * abs x))
    )
    (range 0 $ n_samples - 1)
  where
  n_samples = 44100

  deg = pi / 180.0

myWaveShaper :: WaveShaperSignature MyFloatArrays
myWaveShaper = tWaveShaper (RProxy :: RProxy MyFloatArrays)

myPlay :: PlaySignature MyTracks
myPlay = tPlay (RProxy :: RProxy MyTracks)

scene :: Number -> Behavior (AudioUnit D1)
scene =
  const
    $ pure
        ( speaker'
            ( tAudioWorkletProcessor
                (RProxy :: RProxy MyProcessorsWithParams)
                (SProxy :: SProxy "myProc")
                { foo: 1.0, bar: 1.0 }
                $ myWaveShaper (SProxy :: SProxy "wicked")
                    FourX
                    ( myPlay (SProxy :: SProxy "forest")
                    )
            )
        )

type MyFloatArrays
  = ( wicked :: BrowserFloatArray
    )

type MyTracks
  = ( forest :: BrowserAudioTrack
    )

type MyProcessors a b
  = ( myProc :: a, myOtherProc :: b )

type MyProcessorsWithParams
  = MyProcessors (SymbolListProxy ("foo" :$ ("bar" :$ NilS))) (SymbolListProxy ("baz" :$ NilS))

type MyProcessorsWithUrls
  = MyProcessors String String

type MyEnv
  = ( floatArrays :: Aff (Record MyFloatArrays)
    , tracks :: Aff (Record MyTracks)
    , worklets :: Aff (Record MyProcessorsWithUrls)
    )

env :: Record MyEnv
env =
  { floatArrays:
      do
        wicked <- liftEffect $ makeFloatArray (makeDistortionCurve 400.0)
        pure { wicked }
  , tracks:
      do
        forest <- liftEffect $ makeAudioTrack "https://freesound.org/data/previews/458/458087_8462944-lq.mp3"
        pure { forest }
  , worklets:
      pure
        { myProc: "https://my.js.proc/hello.js"
        , myOtherProc: "https://my.js.proc/hello.js"
        }
  }

floatArrays :: FloatArrays
floatArrays = const $ affableRec env.floatArrays

worklets :: Worklets
worklets = const (affable $ map (toUrlArray (RProxy :: RProxy MyProcessorsWithParams)) env.worklets)

tracks :: Tracks
tracks = const $ affableRec env.tracks

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , tracks = tracks
    , worklets = worklets
    , floatArrays = floatArrays
    }

--------------------------------
__test_tPlay :: forall ch. Pos ch => AudioUnit ch
__test_tPlay =
  ( tPlay
      ( RProxy ::
          RProxy
            ( bassPlease :: BrowserAudioTrack )
      )
  )
    ( SProxy ::
        SProxy "bassPlease"
    )

__test_tPlayBuf :: forall ch. Pos ch => Number -> AudioUnit ch
__test_tPlayBuf =
  ( tPlayBuf
      ( RProxy ::
          RProxy
            ( bassPlease :: BrowserAudioBuffer )
      )
      ( SProxy ::
          SProxy "bassPlease"
      )
  )

__test_tPeriodicOsc :: Number -> AudioUnit D1
__test_tPeriodicOsc =
  ( tPeriodicOsc
      ( RProxy ::
          RProxy
            ( wavey :: BrowserPeriodicWave )
      )
      ( SProxy ::
          SProxy "wavey"
      )
  )

__test_tWaveShaper :: forall ch. Pos ch => Oversample -> AudioUnit ch -> AudioUnit ch
__test_tWaveShaper =
  ( tWaveShaper
      ( RProxy ::
          RProxy
            ( crunch :: BrowserFloatArray )
      )
      ( SProxy ::
          SProxy "crunch"
      )
  )

___wu :: Array String
___wu = toUrlArray (RProxy :: RProxy ( foo :: Void, bar :: Void )) { foo: "", bar: "" }

__test_tAudioWorkletGenerator ::
  AudioUnit D1
__test_tAudioWorkletGenerator =
  ( tAudioWorkletGenerator
      ( RProxy ::
          RProxy
            ( myProc :: SymbolListProxy (ConsSymbol "foo" (ConsSymbol "bar" NilSymbol)) )
      )
      ( SProxy ::
          SProxy "myProc"
      )
      { foo: 1.0, bar: 1.0 }
  )

__test_tAudioWorkletGeneratorT ::
  AudioUnit D1
__test_tAudioWorkletGeneratorT =
  ( tAudioWorkletGeneratorT
      ( RProxy ::
          RProxy
            ( myProc :: SymbolListProxy (ConsSymbol "foo" (ConsSymbol "bar" NilSymbol)) )
      )
      ( SProxy ::
          SProxy "myProc"
      )
      { foo: (AudioParameter { param: 1.0, timeOffset: 0.0 }), bar: (AudioParameter { param: 1.0, timeOffset: 0.0 }) }
  )
