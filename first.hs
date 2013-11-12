-- #1 ready
($$) :: (a -> b) -> a -> b
f $$ x = f x
-- #2 ready
(...) :: (b -> c) -> (a -> b) -> (a -> c)
f ... g = \x -> f(g x)
-- #3 ready
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = (f $$ x) $$ y
-- #4 ready
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = 
   let new_b = mod a b
   in  gcd' b new_b
-- #5 ready
isPrime :: Int -> Bool
isPrime number = iter (number - 1)
		 where iter :: Int -> Bool
                       iter 0 = True
 		       iter i = if (gcd i number) == 1
                                then iter (i-1)
				else False

-- #6 ready
root :: (Double -> Double) -> Double -> Double -> Double -> Double
root f a b eps = let middle = (a + b)/2
                     middleValue = f middle
                     firstValue = f a
                     secondValue = f b
                     test = abs (a - b) 
                 in  if test < eps
                     then f(middle)
                     else
		       if middleValue * firstValue < 0
                       then root f a middle eps
                       else root f middle b eps
-- #7
sequence' :: Int -> Int
sequence' n = truncate $  abs (fromInteger(10^20) * (sin (5 * fromIntegral(n))))