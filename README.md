# cplex-haskell
[![Hackage version](https://img.shields.io/hackage/v/cplex-hs.svg?style=flat)](https://hackage.haskell.org/package/cplex-hs)

Provides low level bindings to the CPLEX Callable Library.
Provides a higher level interface for constructing linear and mip programs.

Includes:

* Support for linear and mixed integer programming
* MIP Callbacks and ability to add cuts
* Write concise models:

```Haskell
 Constraints $ forall onus $ \j -> (sum splitters $ \i -> 1 :# (Phi i j) ) := 1
```

## Change Log
* New MIP additions and callbacks
* MIP additions by [herwigstuetz](https://github.com/herwigstuetz/cplex-haskell)
* Originally developed by [ghorn](https://github.com/ghorn/cplex-haskell) 
