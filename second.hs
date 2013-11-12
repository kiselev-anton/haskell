-- #1 ready №2
myZip :: ([a],[b]) -> [(a,b)]
myZip ([], _) = []
myZip (_, []) = []
myZip (x:xs, y:ys) = (x,y):(myZip (xs,ys))

-- #2 ready
intToDigits :: Int -> [Int]
intToDigits 0 = []
intToDigits number = concat [intToDigits rest, [digit]]
                     where digit = number `mod` 10
             		   rest = number `div` 10
-- #3 ready
digitsToNumber :: [Int] -> Int
digitsToNumber list = iter $ reverse $ list
		where iter :: [Int] -> Int
		      iter [] = 0
		      iter (digit:rest) = digit + 10 * iter rest
-- #4 ready №2
combine :: (Ord a) => [a] -> [a] -> [a]
combine list [] = list
combine [] list = list
combine list1@(x:xs) list2@(y:ys)
	| x < y = x:(combine xs list2)
	| otherwise = y:(combine list1 ys)

-- #5 
-- рекурсия + (хвостовая рекурсия) ready
sumSquares :: (Num a) => [a] -> a
sumSquares [] = 0
sumSquares (x:xs) = x^2 + sumSquares xs

-- явная хвостовая рекурсия ready №2
sumSquaresIter :: (Num a) => [a] -> a
sumSquaresIter list = iter 0 list
	where iter sum [] = sum
	      iter sum (x:xs) = iter (sum + x*x) xs
		
-- foldl ready
sumSquaresFoldl :: (Num a) => [a] -> a
sumSquaresFoldl list = foldl (\sum x -> sum + x*x) 0 list

-- #6 ready
split3 :: [Int] -> ([Int], [Int], [Int])
split3 [] = ([],[],[])
split3 list = (first, second, third)
	where first = filter (\x -> x `mod` 3 == 0) list
	      second = filter (\x -> x `mod` 3 == 1) list
	      third = filter (\x -> x `mod` 3 == 2) list

-- #7 ready
-- сходится к pi^2/6
sum' :: Int -> Double
sum' n = foldl (\sum x -> sum + (1/(x^2))) 0 [1..k]
	where k = fromIntegral n

--sum'' :: Int -> Double
--sum'' n = sum $ map (recip $ fromIntegral $ (^2)) $ [1..n]