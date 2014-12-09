module Integrals where

import Data.Number.IReal
import FAD
import Taylor
import Debug.Trace


integr k n f a b = bsum (map term [1..scale 1 n]) * val (b-a)
          where g x = f (a + (b-a)*x)
                n1 = -(n+1)
                term i = 2 * (sum (take (k `div` 2 + 1) (evens (zipWith (*) ts rs)))
                              + (is!!k - ts!!k) * ((-rk) -+- rk))
                  where ts = taylorCoeffs g (scale (fromInteger (2*i-1)) n1)
                        is = taylorCoeffs g (scale (fromInteger (2*i-1)) n1 +- scale 1 n1)
                evens [] = []
                evens (x:xs) = x : evens (drop 1 xs)
                rs = map (\m -> scale (recip (fromIntegral m)) (n1*m)) [1..]
                rk = rs!!k

integral k d f i = try eps i 
  where
   eps = 10 ^^ (-d)
   evens (x:_:xs) = x : evens xs
   try eps i
    -- |trace (show i ++ ": w=" ++show w) False = undefined
     | w < eps =  approx + ((-w) -+- w)
     | otherwise = try eps2 (lower i -+- m) + try eps2 (m -+- upper i)
         where  eps2 = scale eps (-1)
                m = mid i
                approx = 2 * sum (take (k `div` 2 + 1) (evens (zipWith (*) ts rs)))
                -- w =  upper (abs (prec d $ (derivs f i!!k)/(facs!!k) - ts!!k)) * rs!!k * 2
                w =  upper (abs (prec d $ (ts2!!k - ts!!k))) * rs!!k * 2
                ts = taylorCoeffs f m
                ts2 = taylorCoeffs f i
                rs = zipWith (/) (iterate (*rad i) (rad i)) [1..]

integral2 k d f i = isum' (try eps i [])
  where
   eps = 10 ^^ (-d)
   evens (x:_:xs) = x : evens xs
   try eps i ss
  --   |trace (show i) False = undefined
     | w < eps = approx + ((-w) -+- w) : ss
     | otherwise = try eps2 (lower i -+- m)  
                   (try eps2 (m -+- upper i) ss)
         where  eps2 = scale eps (-1)
                m = mid i
                approx = 2 * sum (take (k `div` 2 + 1) (evens (zipWith (*) ts rs)))
                w =  upper (abs (derivs f i!!k)/(facs!!k) - ts!!k) * (rs!!k) * 2
                ts = taylorCoeffs f m
                rs = zipWith (/) (iterate (*rad i) (rad i)) [1..]

{-
arcLength k eps f = integral k eps g  
    where g x = sqrt (1 + deriv 1 f x ^ 2)

try f k eps i
--     |trace ("k="++show k++" i="++show i) False = undefined
--     | True = (approx+rem,rad rem)
     | rad rem < eps = approx + rem
     | otherwise = try f (min (k+1) 10) (eps/2) (lower i -+- m) + 
                   try f (min (k+1) 10) (eps/2) (m -+- upper i)
         where  m = mid i
                approx = 2 * sum (take (k `div` 2 + 1) (evens (zipWith (*) ts rs)))
                rem =  (deriv k f i/(facs!!k) - ts!!k) * (-rk -+- rk)
                ts = taylorCoeffs f m
                -- is = taylorCoeffs f i
                rs = zipWith (/) (iterate (*rad i) (rad i)) [1..]
                rk = rs!!k

evens (x:_:xs) = x : evens xs
-}