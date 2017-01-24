# sd

[![Build Status](https://travis-ci.org/ocramz/sd.png)](https://travis-ci.org/ocramz/sd)

Symbolic multivariate differentiation in Haskell


## Example

In `sd`, free variables are indexed by integer numbers:

    > ix = 0
    > iy = 1
    > x = Var ix
    > y = Var iy

Suppose we define the expression `3x^2+y^3` :

    > e0 = Const 3 :*: (x :^: Const 2) :+: (y :^: Const 3)
    
    > e0
    ((3.0 * x^2.0) + y^3.0)
    
and a suitable binding environment, relating the free variables to their value:

    > env0 = Env $ IM.fromList [(ix, 5.0), (iy, 2.0)]

We can then require the _gradient_ of `e0` as follows:

    > g = grad env0 e0

    > g 
    fromList [(0,(6.0 * x)),(1,(3.0 * y^2.0))]

Behind the scenes, `grad` differentiates the expression with respect to each of the variables in the binding, which in turn means applying mechanically a number of rewriting and simplification passes. The result is a new IntMap which contains the expressions for the partial derivatives.

Finally, the _numerical value_ of the partial derivatives is obtained by substitution of the bindings:

    > eval env0 <$> g
    fromList [(0,30.0),(1,12.0)]


## Credits and inspiration

I started `sd` after reading this blog by Ben Kovach (from which I also shamelessly ripped the AST and the simplification code for the `Numeric.SD` module):

* https://5outh.blogspot.se/2013/05/symbolic-calculus-in-haskell.html

Currently, I am trying to factor out the recursion from the gradient and evaluation algorithm, using _recursion schemes_ as suggested in Jared Tobin's blog:

* https://jtobin.io/ad-via-recursion-schemes

* https://jtobin.io/sharing-in-haskell-edsls
