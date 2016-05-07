# declarative-anneal

[![Build Status](https://secure.travis-ci.org/jtobin/declarative-anneal.png)](http://travis-ci.org/jtobin/declarative-anneal)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jtobin/declarative-anneal/blob/master/LICENSE)

A simple 'annealing transformer' for
[declarative](https://github.com/jtobin/declarative)-compatible transition
operators.

## Summary

Transition operators can easily be tweaked to operate over an *annealed*
parameter space, which can be useful when sampling from bumpy landscapes with
isolated modes.

This library exports a single `anneal` function that allows one to run a
*declarative*-compatible transition operator over a space that has been
annealed to a specified temperature.

``` haskell
annealingTransition = do
  anneal 0.70 (metropolis 1)
  anneal 0.05 (metropolis 1)
  anneal 0.05 (bernoulliT 0.75 (metropolis 1) (hamiltonian 0.5 20))
  anneal 0.70 (metropolis 1)
  metropolis 1
```

These 'annealed' operators can then just be used like any other:

``` haskell
main :: IO ()
main = withSystemRandom . asGenIO $
  mcmc 10000 [0, 0] annealingTransition himmelblau
```

![trace](https://dl.dropboxusercontent.com/spa/u0s6617yxinm2ca/o8gyycqo.png)

