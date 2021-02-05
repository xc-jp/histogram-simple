# histogram-simple
[![Hackage version](https://img.shields.io/hackage/v/histogram-simple.svg?label=Hackage)](https://hackage.haskell.org/package/histogram-simple)
[![Stackage version](https://www.stackage.org/package/histogram-simple/badge/nightly?label=Stackage)](https://www.stackage.org/package/histogram-simple)

Simple `Data.Map`-based histogram.
A histogram counts occurrences of things, i.e. `Histogram k` represents a mapping `k -> Int`.
Since it is backed by a `Map` from `Data.Map`, it requires `k` to have an `Ord` instance.
