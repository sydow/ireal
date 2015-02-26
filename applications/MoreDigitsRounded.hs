module MoreDigitsRounded where

import Data.Number.IReal.Rounded
import Data.Number.IReal.FAD
import ClenshawRounded


{-

This file contains solutions to the harder versions of problems 15 and 16 in the MoreDigits competition. See file MoreDigits.hs for the other problems. Probably some of these can be solved faster with module Data.Number.IReal.Rounded, but we haven't tried.

-}

p15 k a b c = quad  (\x -> sin (a * cos (b*x+c))) (cpss!!k) (wss!!k) 

{-
ghci MoreDigitsRounded
> import Data.Number.IReal.Rounded
:set +s
:set -XDataKinds
(p15 13 20 20 20 :: Rounded 265) ?? 254
0.71608688553056848008081092230917458779137963176367305213525499620635442269804039553481261557978602143549436015055422778396950304469931102251569240476886870118455258641437467302292796143016953525336574898319125551548729281893014937880076135276345390154853e-2
(21.49 secs, 29031071328 bytes)

The above solves the harder problem using Clenshaw-Curtis quadrature in 2^13+1 points. The correctness has to be argued independently (using well-known error estimates for analytic integrands or, less stringent, by noting the results for 12 and 13 points and relying on geometric convergence.

-}

p16 a b c = deriv c f (recip (sqrt a))
  where f x = sqrt (sin x + recip (sqrt b))

{-

 (p16 4321 543210 200 :: Rounded 25) ?? 20 
-1.39326746432557879171e726
(0.85 secs, 828010472 bytes)
-}
