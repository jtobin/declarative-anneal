{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Control.Monad.Primitive (PrimMonad)
import Numeric.MCMC
import Numeric.MCMC.Anneal (anneal)

himmelblau :: Target [Double]
himmelblau = Target lHimmelblau (Just glHimmelblau) where
  lHimmelblau :: [Double] -> Double
  lHimmelblau [x0, x1] =
    (-1) * ((x0 * x0 + x1 - 11) ^ 2 + (x0 + x1 * x1 - 7) ^ 2)

  glHimmelblau :: [Double] -> [Double]
  glHimmelblau [x, y] =
    let quadFactor0 = x * x + y - 11
        quadFactor1 = x + y * y - 7
        dx = (-2) * (2 * quadFactor0 * x + quadFactor1)
        dy = (-2) * (quadFactor0 + 2 * quadFactor1 * y)
    in  [dx, dy]

annealingTransition = do
  anneal 0.70 (metropolis 1)
  anneal 0.05 (metropolis 1)
  anneal 0.05 (bernoulliT 0.75 (metropolis 1) (hamiltonian 0.5 20))
  anneal 0.70 (metropolis 1)
  metropolis 1

main :: IO ()
main = withSystemRandom . asGenIO $
  mcmc 20 [0, 0] annealingTransition himmelblau

