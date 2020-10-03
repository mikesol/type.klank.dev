module Type.Klank.Dev where

import Prelude
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Typelevel.Num (class Pos, D1)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import FRP.Behavior.Audio (AudioContext, AudioInfo, AudioParameter, AudioUnit, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, Oversample, VisualInfo, loopBuf, loopBufT, loopBufT_, loopBuf_, periodicOsc, periodicOscT, periodicOscT_, periodicOsc_, play, playBuf, playBufT, playBufT_, playBuf_, play_, waveShaper, waveShaper_)
import Foreign.Object (Object, fromHomogeneous)
import Prim.Row (class Cons)
import Type.Data.Row (RProxy)
import Type.Row.Homogeneous (class Homogeneous)

affableRec ::
  forall (a :: # Type) b.
  Homogeneous a b =>
  Aff (Record a) ->
  (Object b -> Effect Unit) ->
  (Error -> Effect Unit) ->
  Effect Unit
affableRec aff res rej =
  launchAff_ do
    result <- try $ aff
    case result of
      Left err -> liftEffect $ rej err
      Right resp -> liftEffect $ res (fromHomogeneous resp)

affable :: forall a. Aff a -> (a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
affable aff res rej =
  launchAff_ do
    result <- try $ aff
    case result of
      Left err -> liftEffect $ rej err
      Right resp -> liftEffect $ res resp

type EnableMicrophone
  = Boolean

type Accumulator accumulator
  = (accumulator -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type Tracks
  = (Object BrowserAudioTrack -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type Buffers
  = AudioContext -> (Object BrowserAudioBuffer -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type FloatArrays
  = (Object BrowserFloatArray -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type PeriodicWaves
  = AudioContext -> (Object BrowserPeriodicWave -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type Main
  = forall accumulator microphones tracks buffers floatArrays periodicWaves.
    accumulator ->
    Int ->
    Int ->
    AudioContext ->
    AudioInfo (Object microphones) (Object tracks) (Object buffers) (Object floatArrays) (Object periodicWaves) ->
    VisualInfo ->
    Effect (Effect Unit)

class HasTrack (env :: # Type) (s :: Symbol)

instance hasTrack ::
  ( Cons s _0 _1 tracks
  , Homogeneous tracks BrowserAudioTrack
  , Cons "tracks" (Aff (Record tracks)) _2 env
  ) =>
  HasTrack env s

tPlay ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasTrack env s =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  AudioUnit ch
tPlay _ s = play (reflectSymbol s)

tPlay_ ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasTrack env s =>
  IsSymbol s =>
  String ->
  RProxy env ->
  SProxy s ->
  AudioUnit ch
tPlay_ n _ s = play_ n (reflectSymbol s)

class HasBuffer (env :: # Type) (s :: Symbol)

instance hasBuffer ::
  ( Cons s _0 _1 buffers
  , Homogeneous buffers BrowserAudioBuffer
  , Cons "buffers" (Aff (Record buffers)) _2 env
  ) =>
  HasBuffer env s

tPlayBuf ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  Number ->
  AudioUnit ch
tPlayBuf _ s = playBuf (reflectSymbol s)

tPlayBuf_ ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  String ->
  RProxy env ->
  SProxy s ->
  Number ->
  AudioUnit ch
tPlayBuf_ n _ s = playBuf_ n (reflectSymbol s)

tPlayBufT ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  AudioParameter Number ->
  AudioUnit ch
tPlayBufT _ s = playBufT (reflectSymbol s)

tPlayBufT_ ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  String ->
  RProxy env ->
  SProxy s ->
  AudioParameter Number ->
  AudioUnit ch
tPlayBufT_ n _ s = playBufT_ n (reflectSymbol s)

tLoopBuf ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  Number ->
  Number ->
  Number ->
  AudioUnit ch
tLoopBuf _ s = loopBuf (reflectSymbol s)

tLoopBuf_ ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  String ->
  RProxy env ->
  SProxy s ->
  Number ->
  Number ->
  Number ->
  AudioUnit ch
tLoopBuf_ n _ s = loopBuf_ n (reflectSymbol s)

tLoopBufT ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  AudioParameter Number ->
  Number ->
  Number ->
  AudioUnit ch
tLoopBufT _ s = loopBufT (reflectSymbol s)

tLoopBufT_ ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  String ->
  RProxy env ->
  SProxy s ->
  AudioParameter Number ->
  Number ->
  Number ->
  AudioUnit ch
tLoopBufT_ n _ s = loopBufT_ n (reflectSymbol s)

class HasPeriodicWave (env :: # Type) (s :: Symbol)

instance hasPeriodicWave ::
  ( Cons s _0 _1 periodicWaves
  , Homogeneous periodicWaves BrowserPeriodicWave
  , Cons "periodicWaves" (Aff (Record periodicWaves)) _2 env
  ) =>
  HasPeriodicWave env s

tPeriodicOsc ::
  forall (env :: # Type) (s :: Symbol).
  HasPeriodicWave env s =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  Number ->
  AudioUnit D1
tPeriodicOsc _ s = periodicOsc (reflectSymbol s)

tPeriodicOsc_ ::
  forall (env :: # Type) (s :: Symbol).
  HasPeriodicWave env s =>
  IsSymbol s =>
  String ->
  RProxy env ->
  SProxy s ->
  Number ->
  AudioUnit D1
tPeriodicOsc_ n _ s = periodicOsc_ n (reflectSymbol s)

tPeriodicOscT ::
  forall (env :: # Type) (s :: Symbol).
  HasPeriodicWave env s =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  AudioParameter Number ->
  AudioUnit D1
tPeriodicOscT _ s = periodicOscT (reflectSymbol s)

tPeriodicOscT_ ::
  forall (env :: # Type) (s :: Symbol).
  HasPeriodicWave env s =>
  IsSymbol s =>
  String ->
  RProxy env ->
  SProxy s ->
  AudioParameter Number ->
  AudioUnit D1
tPeriodicOscT_ n _ s = periodicOscT_ n (reflectSymbol s)

class HasFloatArray (env :: # Type) (s :: Symbol)

instance hasFloatArray ::
  ( Cons s _0 _1 floatArrays
  , Homogeneous floatArrays BrowserFloatArray
  , Cons "floatArrays" (Aff (Record floatArrays)) _2 env
  ) =>
  HasFloatArray env s

tWaveShaper ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasFloatArray env s =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  Oversample ->
  AudioUnit ch ->
  AudioUnit ch
tWaveShaper _ s = waveShaper (reflectSymbol s)

tWaveShaper_ ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasFloatArray env s =>
  IsSymbol s =>
  String ->
  RProxy env ->
  SProxy s ->
  Oversample ->
  AudioUnit ch ->
  AudioUnit ch
tWaveShaper_ n _ s = waveShaper_ n (reflectSymbol s)
