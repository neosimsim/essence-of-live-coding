{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Pulse where

-- base
import Control.Arrow as X
import Control.Concurrent
import Control.Monad (void, forever)
import Control.Monad.Fix
import GHC.Float (int2Float)

-- transformers
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (MonadTrans(lift))

-- pulse-simple
import Sound.Pulse.Simple

-- essence-of-live-coding
import LiveCoding
import Data.Maybe (fromMaybe)

type PulseCell a b = Cell IO a (Float, b)

sampleRate :: Num a => a
sampleRate = 48000

pulseHandle :: Handle IO Simple
pulseHandle = Handle
  { create = simpleNew
      Nothing
      "example"
      Play
      Nothing
      "this is an example application"
      (SampleSpec (F32 LittleEndian) sampleRate 1)
      Nothing
      (Just (BufferAttr Nothing (Just 1000) Nothing Nothing Nothing))
      -- (Just (BufferAttr Nothing (Just 2048) Nothing Nothing (Just 2048)))
  , destroy = \simple -> putStrLn "Destroying pulse" >> simpleFree simple
  }

pulseWrapC :: Int -> PulseCell a () -> Cell (HandlingStateT IO) a ()
pulseWrapC bufferSize cell = proc a -> do
  simple <- handling pulseHandle -< ()
  -- FIXME It remains to test whether sound actually works that way
  nonBlocking False calcAndPushSamples -< Just (simple, a)
  arrM $ lift . threadDelay -< 100
  -- arrM $ lift . threadDelay                  -< round $ int2Float bufferSize * 1000000 / 2000 / int2Float sampleRate -- TODO Tweak for better performance
  returnA -< ()
    where
      calcAndPushSamples = proc (simple, a) -> do
        samplesAndBs <- resampleList cell -< replicate bufferSize a
        let (samples, bs) = unzip samplesAndBs
        arrM $ uncurry simpleWrite -< (simple, samples)

-- Returns the sum between -1 and 1
wrapSum :: (Monad m, Data a, RealFloat a) => Cell m a a
wrapSum = Cell
  { cellState = 0
  , cellStep  = \accum a ->
    let
        (_, !accum')  = properFraction $ accum + a
    in return (accum', accum')
  }

wrapIntegral :: (Monad m, Data a, RealFloat a) => Cell m a a
wrapIntegral = arr (/ sampleRate) >>> wrapSum

modSum :: (Monad m, Data a, Integral a) => a -> Cell m a a
modSum denominator = Cell
  { cellState = 0
  , cellStep  = \accum a -> let accum' = (accum + a) `mod` denominator in return (accum', accum')
  }


clamp :: (Ord a, Num a) => a -> a -> a -> a
clamp lower upper a = min upper $ max lower a

osc :: (Data a, RealFloat a, MonadFix m) => Cell (ReaderT a m) () a
osc = proc _ -> do
  !f <- constM ask -< ()
  !phase <- wrapSum -< f / 48000
  returnA -< sin $ 2 * pi * phase

osc' :: (Data a, RealFloat a, MonadFix m) => Cell m a a
osc' = proc a -> do
  runReaderC' osc -< a `seq` (a, ())

data Note
  = A
  | Bb
  | B
  | C
  | Cis
  | D
  | Dis
  | E
  | F
  | Fis
  | G
  | Gis
  deriving (Enum, Show)

f :: Note -> Float
f note = 220 * (2 ** (fromIntegral (fromEnum note) / 12))

o :: Float -> Float
o = (* 2)
