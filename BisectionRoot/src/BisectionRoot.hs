-- BisectionRoot.hs

module BisectionRoot where

--precondition: f a <= 0 and f b >= 0 and a <= b
bisectionRoot :: (Double -> Double) -> (Double,Double) -> Double
bisectionRoot f (a,b)
 | a==b = a
 | f a == 0 = a
 | f b == 0 = b
 | (succ a) == b =
  if abs (f a) <= abs (f b)
  then a
  else b
 | otherwise =
  if signum (f a) /= signum (f g)
  then bisectionRoot f (a,g)
  else bisectionRoot f (g,b) where
  g
   | t <= a = succ a
   | b <= t = pred b
   | otherwise = t where
    t = (a+b)/2
-- The following t does not seem to converge for some cases
--    t = ((abs $ f b)*a + (abs $ f a)*b)/(afb + afa)

