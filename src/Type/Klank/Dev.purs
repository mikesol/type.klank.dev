module Type.Klank.Dev where

import Prelude
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (snd)
import Data.Typelevel.Num (class Pos, D1)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioInfo, AudioParameter, AudioUnit, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, Exporter, Oversample, VisualInfo, EngineInfo, audioWorkletGenerator, audioWorkletGeneratorT, audioWorkletGeneratorT_, audioWorkletGenerator_, audioWorkletProcessor, audioWorkletProcessorT, audioWorkletProcessorT_, audioWorkletProcessor_, defaultExporter, loopBuf, loopBufT, loopBufT_, loopBuf_, periodicOsc, periodicOscT, periodicOscT_, periodicOsc_, play, playBuf, playBufT, playBufT_, playBuf_, play_, runInBrowser, speaker', waveShaper, waveShaper_)
import Foreign.Object (Object, fromHomogeneous)
import Foreign.Object as O
import Prim.Boolean (False, True, kind Boolean)
import Prim.Ordering (EQ, kind Ordering)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.Symbol (class Compare)
import Type.Data.Boolean (class And, class Or)
import Type.Data.Row (RProxy)
import Type.Row.Homogeneous (class Homogeneous)

data SymbolListProxy (s :: SymbolList)
  = SymbolListProxy

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

type Worklets
  = (Array String) -> (Array String -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type Tracks
  = (Object BrowserAudioTrack) -> (Object BrowserAudioTrack -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type Buffers
  = AudioContext -> (Object BrowserAudioBuffer) -> (Object BrowserAudioBuffer -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type FloatArrays
  = (Object BrowserFloatArray) -> (Object BrowserFloatArray -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type PeriodicWaves
  = AudioContext -> (Object BrowserPeriodicWave) -> (Object BrowserPeriodicWave -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type Run accumulator env
  = forall microphones tracks buffers floatArrays periodicWaves.
    accumulator ->
    AudioContext ->
    EngineInfo ->
    AudioInfo (Object microphones) (Object tracks) (Object buffers) (Object floatArrays) (Object periodicWaves) ->
    VisualInfo ->
    Exporter env ->
    Effect (Effect Unit)

type Klank'' accumulator env
  = { run :: Run accumulator env
    , periodicWaves :: PeriodicWaves
    , floatArrays :: FloatArrays
    , buffers :: Buffers
    , tracks :: Tracks
    , worklets :: Worklets
    , enableMicrophone :: EnableMicrophone
    , accumulator :: Accumulator accumulator
    , exporter :: Exporter env
    , engineInfo :: EngineInfo
    }

type Klank' accumulator
  = Klank'' accumulator Unit

type Klank
  = Klank' Unit

noSound :: Number -> Behavior (AudioUnit D1)
noSound = const $ pure (speaker' zero)

defaultEngineInfo =
  { msBetweenSamples: 20
  , msBetweenPings: 15
  , fastforwardLowerBound: 0.025
  , rewindUpperBound: 0.15
  , initialOffset: 0.1
  } ::
    EngineInfo

klank :: Klank
klank =
  { run: runInBrowser noSound
  , periodicWaves: \_ prev res _ -> res prev
  , floatArrays: \prev res _ -> res prev
  , buffers: \_ prev res _ -> res prev
  , tracks: \prev res _ -> res prev
  , worklets: \prev res _ -> res prev
  , enableMicrophone: false
  , accumulator: \res _ -> res unit
  , exporter: defaultExporter
  , engineInfo: defaultEngineInfo
  }

class HasTrack (env :: # Type) (s :: Symbol)

instance hasTrack ::
  ( Cons s _0 _1 tracks
  , Homogeneous tracks BrowserAudioTrack
  ) =>
  HasTrack tracks s

tPlay ::
  forall (env :: # Type) (s :: Symbol) ch.
  Pos ch =>
  HasTrack env s =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  AudioUnit ch
tPlay _ s = play (reflectSymbol s)

type PlaySignature (myTracks :: # Type)
  = forall ch s a t.
    HasTrack myTracks s =>
    Pos ch =>
    Cons s a t myTracks =>
    IsSymbol s =>
    SProxy s ->
    AudioUnit ch

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

type Play_Signature (myTracks :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasTrack myTracks s =>
    Cons s a t myTracks =>
    IsSymbol s =>
    String ->
    SProxy s ->
    AudioUnit ch

class HasBuffer (env :: # Type) (s :: Symbol)

instance hasBuffer ::
  ( Cons s _0 _1 buffers
  , Homogeneous buffers BrowserAudioBuffer
  ) =>
  HasBuffer buffers s

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

type PlayBufSignature (myBuffers :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    SProxy s ->
    Number ->
    AudioUnit ch

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

type PlayBuf_Signature (myBuffers :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    String ->
    SProxy s ->
    Number ->
    AudioUnit ch

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

type PlayBufTSignature (myBuffers :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    SProxy s ->
    AudioParameter Number ->
    AudioUnit ch

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

type PlayBufT_Signature (myBuffers :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    String ->
    SProxy s ->
    AudioParameter Number ->
    AudioUnit ch

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

type LoopBufSignature (myBuffers :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    SProxy s ->
    Number ->
    Number ->
    Number ->
    AudioUnit ch

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

type LoopBuf_Signature (myBuffers :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    String ->
    SProxy s ->
    Number ->
    Number ->
    Number ->
    AudioUnit ch

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

type LoopBufTSignature (myBuffers :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    SProxy s ->
    AudioParameter Number ->
    Number ->
    Number ->
    AudioUnit ch

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

type LoopBufT_Signature (myBuffers :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    String ->
    SProxy s ->
    AudioParameter Number ->
    Number ->
    Number ->
    AudioUnit ch

class HasPeriodicWave (env :: # Type) (s :: Symbol)

instance hasPeriodicWave ::
  ( Cons s _0 _1 periodicWaves
  , Homogeneous periodicWaves BrowserPeriodicWave
  ) =>
  HasPeriodicWave periodicWaves s

tPeriodicOsc ::
  forall (env :: # Type) (s :: Symbol).
  HasPeriodicWave env s =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  Number ->
  AudioUnit D1
tPeriodicOsc _ s = periodicOsc (reflectSymbol s)

type PeriodicOscSignature (periodicWaves :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasPeriodicWave periodicWaves s =>
    Cons s a t periodicWaves =>
    IsSymbol s =>
    SProxy s ->
    Number ->
    AudioUnit ch

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

type PeriodicOsc_Signature (periodicWaves :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasPeriodicWave periodicWaves s =>
    Cons s a t periodicWaves =>
    IsSymbol s =>
    String ->
    SProxy s ->
    Number ->
    AudioUnit ch

tPeriodicOscT ::
  forall (env :: # Type) (s :: Symbol).
  HasPeriodicWave env s =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  AudioParameter Number ->
  AudioUnit D1
tPeriodicOscT _ s = periodicOscT (reflectSymbol s)

type PeriodicOscTSignature (periodicWaves :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasPeriodicWave periodicWaves s =>
    Cons s a t periodicWaves =>
    IsSymbol s =>
    SProxy s ->
    AudioParameter Number ->
    AudioUnit ch

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

type PeriodicOscT_Signature (periodicWaves :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasPeriodicWave periodicWaves s =>
    Cons s a t periodicWaves =>
    IsSymbol s =>
    String ->
    SProxy s ->
    AudioParameter Number ->
    AudioUnit ch

class HasFloatArray (env :: # Type) (s :: Symbol)

instance hasFloatArray ::
  ( Cons s _0 _1 floatArrays
  , Homogeneous floatArrays BrowserFloatArray
  ) =>
  HasFloatArray floatArrays s

type WaveShaperSignature (myFloatArrays :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasFloatArray myFloatArrays s =>
    Cons s a t myFloatArrays =>
    IsSymbol s =>
    SProxy s ->
    Oversample ->
    AudioUnit ch ->
    AudioUnit ch

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

type WaveShaper_Signature (myFloatArrays :: # Type)
  = forall ch s a t.
    Pos ch =>
    HasFloatArray myFloatArrays s =>
    Cons s a t myFloatArrays =>
    IsSymbol s =>
    String ->
    SProxy s ->
    Oversample ->
    AudioUnit ch ->
    AudioUnit ch

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

foreign import kind SymbolList

foreign import data ConsSymbol :: Symbol -> SymbolList -> SymbolList

infixr 4 type ConsSymbol as :$

foreign import data NilSymbol :: SymbolList

type NilS
  = NilSymbol

-- A spec for an audio worklet
-- The name
-- The url
-- A list of params
foreign import data WorkletSpec :: Symbol -> Symbol -> SymbolList -> Type

class WorkletUrls (spec :: # Type) (urls :: # Type) where
  toUrlArray :: RProxy spec -> Record urls -> Array String

class HasAllKeys (needles :: RowList) (haystack :: RowList)

instance hasAllKeysNil :: HasAllKeys Nil a

instance hasAllKeysCons ::
  ( Compare s0 s1 EQ
  , HasAllKeys tail0 tail1
  ) =>
  HasAllKeys (Cons s0 _0 tail0) (Cons s1 _1 tail1)

class IsEq (o :: Ordering) (b :: Boolean) | o -> b

instance isEqEq :: IsEq EQ True
else instance isEqOther :: IsEq a False

class HasASymbol (needle :: Symbol) (haystack :: RowList) (b :: Boolean) | needle haystack -> b

instance hasASymbolsNil :: HasASymbol s Nil False

instance hasASymbolCons ::
  ( Compare s h e
  , IsEq e b0
  , HasASymbol s tail b1
  , Or b0 b1 b
  ) =>
  HasASymbol s (Cons h _0 tail) b

class HasAllSymbols (needles :: SymbolList) (haystack :: RowList) (b :: Boolean) | needles haystack -> b

instance hasAllSymbolsNil :: HasAllSymbols NilSymbol a True

instance hasAllSymbolsCons ::
  ( HasASymbol s0 haystack b0
  , HasAllSymbols tail0 haystack b1
  , And b0 b1 b
  ) =>
  HasAllSymbols (ConsSymbol s0 tail0) haystack b

instance workletUrls ::
  ( RowToList spec specAsList
  , RowToList urls urlsAsList
  , HasAllKeys specAsList urlsAsList
  , Homogeneous urls String
  ) =>
  WorkletUrls spec urls where
  toUrlArray _ = map snd <<< O.toUnfoldable <<< fromHomogeneous

class HasAudioWorklet (env :: # Type) (s :: Symbol) (params :: # Type)

instance hasAudioWorkletProcessor ::
  ( Cons s (SymbolListProxy slist) _1 worklets
  , RowToList params paramsAsList
  , HasAllSymbols slist paramsAsList True
  ) =>
  HasAudioWorklet worklets s params

tAudioWorkletGenerator ::
  forall (env :: # Type) (s :: Symbol) (params :: # Type).
  HasAudioWorklet env s params =>
  Homogeneous params Number =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  (Record params) ->
  AudioUnit D1
tAudioWorkletGenerator _ s p = audioWorkletGenerator (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletGenerator_ ::
  forall (env :: # Type) (s :: Symbol) (params :: # Type).
  HasAudioWorklet env s params =>
  Homogeneous params Number =>
  IsSymbol s =>
  String ->
  RProxy env ->
  SProxy s ->
  (Record params) ->
  AudioUnit D1
tAudioWorkletGenerator_ n _ s p = audioWorkletGenerator_ n (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletGeneratorT ::
  forall (env :: # Type) (s :: Symbol) (params :: # Type).
  HasAudioWorklet env s params =>
  Homogeneous params (AudioParameter Number) =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  (Record params) ->
  AudioUnit D1
tAudioWorkletGeneratorT _ s p = audioWorkletGeneratorT (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletGeneratorT_ ::
  forall (env :: # Type) (s :: Symbol) (params :: # Type).
  HasAudioWorklet env s params =>
  Homogeneous params (AudioParameter Number) =>
  IsSymbol s =>
  String ->
  RProxy env ->
  SProxy s ->
  (Record params) ->
  AudioUnit D1
tAudioWorkletGeneratorT_ n _ s p = audioWorkletGeneratorT_ n (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletProcessor ::
  forall (env :: # Type) (s :: Symbol) (params :: # Type) ch.
  Pos ch =>
  HasAudioWorklet env s params =>
  Homogeneous params Number =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  (Record params) ->
  AudioUnit ch ->
  AudioUnit ch
tAudioWorkletProcessor _ s p = audioWorkletProcessor (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletProcessor_ ::
  forall (env :: # Type) (s :: Symbol) (params :: # Type) ch.
  Pos ch =>
  HasAudioWorklet env s params =>
  Homogeneous params Number =>
  IsSymbol s =>
  String ->
  RProxy env ->
  SProxy s ->
  (Record params) ->
  AudioUnit ch ->
  AudioUnit ch
tAudioWorkletProcessor_ n _ s p = audioWorkletProcessor_ n (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletProcessorT ::
  forall (env :: # Type) (s :: Symbol) (params :: # Type) ch.
  Pos ch =>
  HasAudioWorklet env s params =>
  Homogeneous params (AudioParameter Number) =>
  IsSymbol s =>
  RProxy env ->
  SProxy s ->
  (Record params) ->
  AudioUnit ch ->
  AudioUnit ch
tAudioWorkletProcessorT _ s p = audioWorkletProcessorT (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletProcessorT_ ::
  forall (env :: # Type) (s :: Symbol) (params :: # Type) ch.
  Pos ch =>
  HasAudioWorklet env s params =>
  Homogeneous params (AudioParameter Number) =>
  IsSymbol s =>
  String ->
  RProxy env ->
  SProxy s ->
  (Record params) ->
  AudioUnit ch ->
  AudioUnit ch
tAudioWorkletProcessorT_ n _ s p = audioWorkletProcessorT_ n (reflectSymbol s) (fromHomogeneous p)
