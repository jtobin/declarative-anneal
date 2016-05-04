{-# LANGUAGE RecordWildCards #-}

module Numeric.MCMC.Anneal (
    anneal

  -- needed to build operators
  , module Control.Monad.Primitive
  ) where

import Control.Monad.Trans.State.Strict (StateT, get, put, modify)
import Data.Sampling.Types (Transition, Chain(..), Target(..))
import Control.Monad.Primitive (PrimMonad, PrimState)

anneal
  :: (Monad m, Functor f)
  => Double
  -> Transition m (Chain (f Double) b)
  -> Transition m (Chain (f Double) b)
anneal invTemp baseTransition
  | invTemp < 0 = error "anneal: invalid temperture"
  | otherwise = do
      Chain {..} <- get
      let annealedTarget = annealer invTemp chainTarget
      modify $ useTarget annealedTarget
      baseTransition
      modify $ useTarget chainTarget

annealer :: Functor f => Double -> Target (f Double) -> Target (f Double)
annealer invTemp target = Target annealedL annealedG where
  annealedL xs = invTemp * lTarget target xs
  annealedG    =
    case glTarget target of
      Nothing -> Nothing
      Just g  -> Just (fmap (* invTemp) . g)

useTarget :: Target a -> Chain a b -> Chain a b
useTarget newTarget Chain {..} =
  Chain newTarget (lTarget newTarget chainPosition) chainPosition chainTunables

schedule :: (Enum a, Floating a) => Int -> [a]
schedule n = initial ++ replicate d 1.0 where
  m = floor $ fromIntegral n / 5
  d = n - m
  initial = map (/ fromIntegral m) (logspace m 1 (fromIntegral m))

linspace :: (Enum a, Fractional a) => Int -> a -> a -> [a]
linspace n a b = map ((+) a . scale) [0..fromIntegral n - 1] where
  scale = (*) ((b - a) / fromIntegral (n - 1))

logspace :: (Enum a, Floating a) => Int -> a -> a -> [a]
logspace n a b = map (exp . (*) (log 10)) lins where
  lins = linspace n (logBase 10 a) (logBase 10 b)

