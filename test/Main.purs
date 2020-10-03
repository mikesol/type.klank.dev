module Test.Main where

import Prelude
import Data.Array (range)
import Data.Int (toNumber)
import Data.Symbol (SProxy(..))
import Data.Typelevel.Num (class Pos, D1)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, Oversample(..), gain', makeAudioTrack, makeFloatArray, runInBrowser, speaker')
import Math (pi, abs)
import Type.Data.Row (RProxy(..))
import Type.Klank.Dev (FloatArrays, Main, Tracks, affableRec, tPeriodicOsc, tPlay, tPlayBuf, tWaveShaper)

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

scene :: Number -> Behavior (AudioUnit D1)
scene =
  const
    $ pure
        ( speaker'
            ( gain' 0.2
                $ tWaveShaper
                    (RProxy :: RProxy MyEnv)
                    (SProxy :: SProxy "wicked")
                    FourX
                    ( tPlay
                        (RProxy :: RProxy MyEnv)
                        (SProxy :: SProxy "forest")
                    )
            )
        )

type MyFloatArrays
  = ( wicked :: BrowserFloatArray
    )

type MyTracks
  = ( forest :: BrowserAudioTrack
    )

type MyEnv
  = ( floatArrays :: Aff (Record MyFloatArrays)
    , tracks :: Aff (Record MyTracks)
    )

env =
  { floatArrays:
      do
        wicked <- liftEffect $ makeFloatArray (makeDistortionCurve 400.0)
        pure { wicked }
  , tracks:
      do
        forest <- liftEffect $ makeAudioTrack "https://freesound.org/data/previews/458/458087_8462944-lq.mp3"
        pure { forest }
  } ::
    (Record MyEnv)

floatArrays :: FloatArrays
floatArrays = affableRec env.floatArrays

tracks :: Tracks
tracks = affableRec env.tracks

main :: Main
main = runInBrowser scene

--------------------------------
__test_tPlay :: forall ch. Pos ch => AudioUnit ch
__test_tPlay =
  ( tPlay
      ( RProxy ::
          RProxy
            ( tracks ::
                Aff
                  { bassPlease :: BrowserAudioTrack
                  }
            )
      )
      ( SProxy ::
          SProxy "bassPlease"
      )
  )

__test_tPlayBuf :: forall ch. Pos ch => Number -> AudioUnit ch
__test_tPlayBuf =
  ( tPlayBuf
      ( RProxy ::
          RProxy
            ( buffers ::
                Aff
                  { bassPlease :: BrowserAudioBuffer
                  }
            )
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
            ( periodicWaves ::
                Aff
                  { wavey :: BrowserPeriodicWave
                  }
            )
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
            ( floatArrays ::
                ( Aff
                    { crunch :: BrowserFloatArray
                    }
                )
            )
      )
      ( SProxy ::
          SProxy "crunch"
      )
  )
