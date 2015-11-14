{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Arc where

import Control.Arrow
import qualified Data.Foldable as F
import Data.Monoid ((<>), Sum(..))
import qualified Data.Sequence as S
import Data.Time
import System.Random
import Reflex
import Reflex.Dom
import Reflex.Dom.Class
import Reflex.Dom.Contrib.Widgets.Svg
import Reflex.Dom.Contrib.Time
import Reflex.Dom.Contrib.Utils


main :: IO ()
main = do
  r0 <- getStdGen
  r1 <- getStdGen
  t0 <- getCurrentTime
  mainWidget $ firstPair r0 r1 t0


data Epsp = Epsp {
    epsp_t   :: Double -> Double
    -- ^ Function from time-since epsp to synaptic potential
  , epsp_dur :: Double
    -- ^ Duration after which epsp is close enough to 0 that we can throw it out
  }


data Neurotransmitter = Glutamate | Gaba


data QuantalContent = QC {
    qcTransmitter :: Neurotransmitter
  , qcNQuanta     :: Double
  }

newtype DendriticResponse =
  DR { unDR :: QuantalContent -> Epsp }


-- | Utility function for clearing expired Epsp's from a sequence
pruneEpsps :: UTCTime
           -> S.Seq(UTCTime, Epsp)
           -> S.Seq(UTCTime, Epsp)
pruneEpsps t = S.dropWhileR
  (\(t_e, e) -> t > (addUTCTime (realToFrac $ epsp_dur e) t_e ))


-- | A simple exponential Epsp Generator
expEpsp :: Double -> Double -> Epsp
expEpsp vPeak tau =
  Epsp (\t -> vPeak * exp(negate (t/tau)))
       (5*tau)


-- | A simple response function: get excited for Glutamate
--   and inhibited by Gaba
simpleResponse :: DendriticResponse
simpleResponse =
  DR $ \case
          QC Glutamate x -> expEpsp (0.02 * x)         0.2
          QC Gaba      x -> expEpsp (negate $ 0.1 * x) 0.1


-- | A sum over Epsp's stored in a Sequence at a specific time
sumEpsps :: S.Seq (UTCTime, Epsp) -> UTCTime -> Double
sumEpsps epspSeq tNow =
--  getSum $ F.foldMap (\(e_t :: UTCTime, e) ->
--    Sum (epsp_t e $ realToFrac $ diffUTCTime tNow e_t :: Double)) epspSeq (Sum 0 :: Sum Double)
  let epspLevels = ffor epspSeq $ \(e_t,Epsp f _) -> f (realToFrac $ diffUTCTime tNow e_t)
  in  F.foldl' (+) 0 epspLevels


-- | Include a new time-tagged epsp, and drop the ones that hav expired
--   We pass the time of the new Epsp instead of the proper 'current time'
--   for implementer's convenience.
updateEpsps :: (UTCTime, Epsp) -> S.Seq (UTCTime, Epsp) -> S.Seq (UTCTime, Epsp)
updateEpsps x xs =  x S.<| (pruneEpsps (fst x) xs)

-- | UpdateEpsps and a time-now tag
updateEpspsTimed :: (UTCTime, (UTCTime,Epsp)) -> (UTCTime, S.Seq (UTCTime,Epsp))
                                              -> (UTCTime, S.Seq (UTCTime,Epsp))
updateEpspsTimed (t,x) (_,xs) = (t, updateEpsps x xs)

data Neuron t = Neuron
  { inputs      :: [(Dynamic t DendriticResponse, Event t (UTCTime, QuantalContent))]
  , baseline    :: Dynamic t Double
  , position    :: Dynamic t (Double, Double)
  , spikes      :: Event t (UTCTime, QuantalContent)
  , membraneV   :: Dynamic t Double
  }

clampPositive :: Double -> Double
clampPositive = max 0

neuronWidget :: (MonadWidget t m, RandomGen g)
             => g
             -> UTCTime
             -> [(Dynamic t DendriticResponse, Event t (UTCTime, QuantalContent))]
             -> Dynamic t Double
             -> Dynamic t (Double, Double)
             -> Bool
             -> m (Neuron t)
neuronWidget g t0 inSpikes thresh pos b = mdo

  es <- fmap _tickInfo_lastUTC <$> poissonLossy g 10 t0
  let extras = (constDyn simpleResponse, fmap (\ti -> (ti, QC Glutamate 1)) es)
  let x = case b of
        True -> extras:inSpikes
        False -> inSpikes

  let (timingRNG, pickingRNG) = split g
      inEpspEvents = ffor x $ \(fn,qc) ->
        attachWith (\f (t,q) -> (t,(unDR f) q)) (current fn) qc -- [Event t (Time,Epsp)]

  epspBank <- foldDyn updateEpsps S.empty (leftmost inEpspEvents)

  -- Event t (rnd(0,1), TickInfo)
  samplingTics <- zipListWithEvent (,) (randomRs (0,1) pickingRNG) =<<
                  poissonLossy timingRNG 100 t0

  -- Event t (Event t (rnd(0,1), TickInfo, MembraneVoltage from EPSPs))
  let vmSamples = attachDynWith
                  (\epsps (r,tInfo) -> (r,tInfo,sumEpsps epsps (_tickInfo_lastUTC tInfo)))
                  epspBank samplingTics
  -- vmSamples = ffor samplingTics $ \(r,sTick) ->
  --      (r,sTick, sumEpsps (leftmost inEpspEvents) (_tickInfo_lastUTC sTick))

  -- Event t (rnd(0,1), TickInfo, SpikeRate)
  let spikeRate = ffor vmSamples $ \(r,tI,vM) ->
        (r,tI,max 0 (100 * ((-0.06) + vM - (-0.06))))

      -- Event t TickInfo
      threshCrossingSamples =
        fmap (\(a,b,c) -> b)
        $ ffilter (\(r,t,sRate) -> sRate >= r * 100) spikeRate

  let axonSpikes = ffor threshCrossingSamples $ \tick ->
       (_tickInfo_lastUTC tick, QC Glutamate 10)

  circAttrs <- forDyn pos $ \(x,y) -> "r"     =: "10"
                                   <> "cx"    =: show x
                                   <> "cy"    =: show y
                                   <> "color" =: "red"
  svgDynAttr "circle" circAttrs $ return ()

  dynVm <- holdDyn 0 $ fmap (\(a,b,c) -> c) vmSamples

  return (Neuron inSpikes (constDyn 0) pos axonSpikes dynVm)

  -- spontaneousEvents <- inhomogeneousPoisson g (current $ spikeRate) 100 t0
  -- inTimedEpspEvents = attach tNow (leftmost inEpspEvents)

  -- epspBank    <- foldDyn updateEpspsTimed (nilTime, S.empty) inTimedEpspEvents

firstPair :: (RandomGen g, MonadWidget t m) => g -> g -> UTCTime -> m ()
firstPair r0 r1 t0 = do
  (n0,n1,n2) <- svg "svg" $ do
    n0 <- neuronWidget r0 t0 [] (constDyn 1) (constDyn (10,10)) True
    n1 <- neuronWidget r1 t0 [(constDyn simpleResponse, spikes n0)] (constDyn 1) (constDyn (50,10)) False
    n2 <- neuronWidget r1 t0 [] (constDyn 1) (constDyn (110,10)) False
    return (n0,n1,n2)
  el "div" $ do
    text "vM: "
    dynText =<< mapDyn show (membraneV n0)
  el "div" $ do
    text "vM: "
    dynText =<< mapDyn show (membraneV n1)
  el "div" $ do
    text "vM: "
    dynText =<< mapDyn show (membraneV n2)
  dynText =<< mapDyn (show :: Int -> String) =<< count (spikes $ n1)

  return ()


nilTime :: UTCTime
nilTime = UTCTime (fromGregorian 2015 11 05) 0
