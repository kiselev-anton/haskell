import Data.Ratio

-- #1 ready
sinN :: Int -> Double
sinN n = iterate sin 1 !! n

-- #2 
-- TODO: to hex
numToDigits :: Int -> [Int]
numToDigits n
        | n == 0 = []
        | otherwise = numToDigits(n `div` 10) ++ [n `mod` 10]
digits :: Int -> [Int]
digits n = concat $ map numToDigits [1..n]

-- #3 ready
evenDoubles :: [Int] -> [Int]
evenDoubles list = concat $ map processing list
    where processing n =
            if n `mod` 2 == 0 then [n,n]
            else [n]

-- #4 ready
sumSeries :: (Integer -> Rational) -> Integer -> Rational -> Rational
sumSeries func n0 eps = 
    -- foldl (+) 0 
    sum $ takeWhile ((>eps) . abs) $ map func [1..]

-- sumSeries (\n -> 1 % n) 1 (1 % 10
-- sumSeries (\n -> 1 % (n*n)) 1 (1 % 10)

-- #5 ready
smallestN :: Double
smallestN = head $ dropWhile (<0.9999) $ map (\n -> sin n*n) [1..]

-- #6 ready
triangulars :: Int -> [Int]
triangulars n = scanl (+) 1 [2..n]

-- #7 ready /2
factorial n = foldl (*) 1 [2..n]
combination n k = factorial n `div` (factorial k * factorial (n - k))

newCombination n 1 = n
newCombination n 0 = 1
newCombination n k 
    | k > n = 0
    | otherwise = newCombination (n-1) (k-1) + newCombination (n-1) k

newton :: Integer -> [Integer]
newton n = map (combination n) [0..n]
newNewton :: Integer -> [Integer]
newNewton 1 = [1,1]
newNewton n = foldl (+) (newNewton (n-1)) ([0] ++ (tail $ newNewton (n-1)))

-- #8
erato :: Int -> [Int]
erato n = primes [2..n]

primes [] = []
primes (x:xs) = x : filter (\n -> n `mod` x /= 0) (primes xs)