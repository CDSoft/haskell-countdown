# License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see http://www.gnu.org/licenses/.

# Introduction

“Countdown” is a game where the players have to take numbers and
operations ($+$, $-$, $*$, $/$) and find a way to arrive to a given
result.

For instance, with 25, 50, 75, 100, 3 and 6 can you compute 952?

Well this one is a bit difficult to find. Let’s see if Haskell can find
a solution.

# Operations

Let’s define a type to represent operations.

The type `Expr` is either a number (an integer) or an operation ($+$,
$-$, $*$, $/$) with two operands. We can add a third data which is the
result of the operation to avoid recalculating it several times.

``` haskell

module Main where

import Data.List
import System.Environment
import Control.Monad

data Expr = Number Integer
          | Add Expr Expr Integer
          | Sub Expr Expr Integer
          | Mul Expr Expr Integer
          | Div Expr Expr Integer
```

The result of an operation is retrieved by `val`:

``` haskell

val (Number x) = x
val (Add _ _ x) = x
val (Sub _ _ x) = x
val (Mul _ _ x) = x
val (Div _ _ x) = x
```

`printOpts` prints operations in a human readable way:

``` haskell
printOps (Number n) = print n
printOps e = (putStrLn . sh) e
    where sh (Number x) = ""
          sh (Add x y v) = sh' x ++ sh' y ++ show (val x) ++ " + " ++ show (val y) ++ " = " ++ show v
          sh (Sub x y v) = sh' x ++ sh' y ++ show (val x) ++ " - " ++ show (val y) ++ " = " ++ show v
          sh (Mul x y v) = sh' x ++ sh' y ++ show (val x) ++ " * " ++ show (val y) ++ " = " ++ show v
          sh (Div x y v) = sh' x ++ sh' y ++ show (val x) ++ " / " ++ show (val y) ++ " = " ++ show v
          sh' (Number _) = ""
          sh' e = sh e ++ "; "
```

# Solver

The solver is quiet simple. It generates all the possible computations.
They will later be filtered to keep the best one.

A game is a list of expressions. Initially the game contains only
numbers. Each step consists in taking two expressions and an operation
and returning a new list with the new expression and the remaining ones.

So a list with $N$ expressions will produce a list of lists with $N-1$
expressions, the first one being built from two expressions of the
original list. Each produced list is a possible step from the initial
list.

``` haskell

steps :: [Expr] -> [[Expr]]
steps es = [ e:es'' | (x, es' ) <- extract es
                    , (y, es'') <- extract es'
                    , e <- ops x y
                    ]
    where
        extract [] = []
        extract (e:es) = (e, es) : [ (e', e:es') | (e', es') <- extract es ]
```

We can now play “one step”. We have now to play all the possible steps.
The function `steps'` takes a list of games (a list of expression lists)
and append all the possible steps from all the games.

This way we recursively build a list with all the possible expressions
reachable from the initial numbers. Thanks to lazyness, the generation
will stop when a solution is found.

``` haskell

steps' :: [[Expr]] -> [[Expr]]
steps' [] = []
steps' ess = ess ++ steps' (concatMap steps ess)
```

There are some constraints on operations. `ops` takes two expressions
and generates the possible expressions.

Starting from $x$ and $y$ we generate:

- $x+y$ if $x \ne 0 \land y \ne 0$ otherwise $x+y$ would not be a new
  number and the operation is useless. we also generate $x+y$ only if
  $x \ge y$ to avoid generating $x+y$ and $y+x$ which would double the
  number of expressions to test.
- $x-y$ if $x \ne 0 \land y \ne 0$ otherwise $x-y$ would not be a new
  number and the operation is useless. we also generate $x-y$ only if
  $y \lt x$ to avoid generating $0$ or negative numbers.
- $x*y$ if $x \gt 1 \land y \gt 1 \land x \ge y$ for the same reasons
- $x/y$ if $x \gt 1 \land y \gt 1 \land y|x$ for obvious reasons too
  (the result of the division must be an integral number).

When an expression is built, its value is also computed.

``` haskell

ops :: Expr -> Expr -> [Expr]
ops x y = [ Add x y (x' + y')     | 0<y', y'<=x' ] ++
          [ Sub x y (x' - y')     | 0<y', y'< x' ] ++
          [ Mul x y (x' * y')     | 1<y', y'<=x' ] ++
          [ Div x y (x' `div` y') | 1<y', x' `mod` y' == 0 ]
    where
        x' = val x
        y' = val y
```

The solver is finally a function that takes all the possible expressions
that can be built from the initial numbers and filters to best ones. It
records the best solutions encountered since the beginning of the list
and emit the current one if it is closer to the expected number. When
the expected number is found, the remaining expressions are ignored.

`solve` take the number `n` to find, the value of the best solution
encountered upto now (`best`) and the list of expressions (`e:es`):

- if $e=n$ then we found an exact solution. It is returned and the
  search stops here.
- if $\lvert n-e \rvert \lt \lvert n-best \rvert$ then we found a better
  approximate solution. It is returned but the search continues.
- Other cases are worse solutions and are ignored.

The last item of the list returned by `solve` is then the best (or
exact) solution.

As the solution candidates are generated in increasing size order, the
first one that is found is also the one that requires the minimal number
of operations.

``` haskell

solve n best (e:es) | val e == n                       = [e]
                    | abs (n - val e) < abs (n - best) = e : solve n (val e) es
                    | otherwise                        = solve n best es
solve n best [] = []
```

The `main` function takes the initial values and the target one. It uses
`solve` and prints all the expressions it returns. The last one is the
best solution.

``` haskell

main = do
    args <- getArgs
    let n = read $ last args
    let ns = map read $ init args
    let es = concat $ steps' [map Number ns]
    putStrLn $ show n ++ " with " ++ show ns ++ " ?"
    forM_ (solve n (-n) es) printOps
```

# Examples

1.  How to find 102 with 25, 50, 75, 100, 3, 6 ? easy!

<!-- -->

    $ runhaskell countdown.lhs 25 50 75 100 3 6 102

    102 with [25,50,75,100,3,6] ?
    25
    50
    75
    100
    100 + 3 = 103
    50 / 25 = 2; 100 + 2 = 102

2.  How to find 952 with 25, 50, 75, 100, 3, 6 ???

<!-- -->

    $ runhaskell countdown.lhs 25 50 75 100 3 6 952

    952 with [25,50,75,100,3,6] ?
    25
    50
    75
    100
    25 * 6 = 150
    50 * 25 = 1250
    25 - 3 = 22; 50 * 22 = 1100
    25 - 6 = 19; 50 * 19 = 950
    25 - 6 = 19; 50 * 19 = 950; 950 + 3 = 953
    75 * 3 = 225; 100 + 6 = 106; 225 * 106 = 23850; 23850 - 50 = 23800; 23800 / 25 = 952

# Source

The Haskell source code is here: [countdown.lhs](countdown.lhs)
