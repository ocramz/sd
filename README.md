# sd

[![Build Status](https://travis-ci.org/ocramz/sd.png)](https://travis-ci.org/ocramz/sd)

Symbolic multivariate differentiation in Haskell


## Example

In `sd`, free variables are indexed by integer numbers:

    ix = 0
    iy = 1
    x = Var ix
    y = Var iy

Suppose we define the expression `3x^2+y^3` :

    e0 = Const 3 :*: (x :^: Const 2) :+: (y :^: Const 3)
    
and a suitable binding environment, relating the free variables to their value:

    env0 = Env $ IM.fromList [(ix, 5.0), (iy, 2.0)]



## Credits and inspiration

I started `sd` after reading this blog by Ben Kovach (from which I also shamelessly ripped the AST and the simplification code for the `Numeric.SD` module)

* https://5outh.blogspot.se/2013/05/symbolic-calculus-in-haskell.html

* https://jtobin.io/ad-via-recursion-schemes

* https://jtobin.io/sharing-in-haskell-edsls
