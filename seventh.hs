import System.Random
import Control.Applicative
import Control.Monad.Writer
import Data.List
-- #1 ready
-- Applicative functors
{-
cartesianProduct1 :: [a] -> [a] -> [(a,a)]
cartesianProduct1 x y = (,) <$> x <*> y

-- List comprehension
cartesianProduct2 :: [a] -> [a] -> [(a,a)]
cartesianProduct2 xs ys = [(x,y) | x <- xs, y <- ys]

-- Bicycle
cartesianProduct3 :: [a] -> [a] -> [(a,a)]
cartesianProduct3 [] xs = []
cartesianProduct3 (x:xs) ys = map (\y -> (x, y) ys ++ cartesianProduct3 xs ys

-- Monads
cartesianProduct4 :: [a] -> [a] -> [(a,a)]
cartesianProduct4 list1 list2 = list1 >>= (\a -> map (\x -> (a,x) list2)

--Monads2
cartesianProduct5 :: [a] -> [a] -> [(a,a)]
cartesianProduct5 list1 list2 = do
	a <- list1
	b <- list2
	return (a,b)
-}
-- #2 ready
-- a=1, b=2, c,d=10, e=1,11,21
-- String = [Char]

-- Использование бинарных деревьев даёт выйгрыш в паре миллисекунд по сравнению с nub
data (Eq a, Ord a) => Tree a = Empty | Node {element::a, left::Tree a, right::Tree a}
		deriving(Eq, Show)

empty' :: Tree a -> Bool
empty' Empty = True
empty' _ = False

add :: (Eq a, Ord a) => a -> Tree a -> Tree a
add value tree
		| empty' tree = (Node value Empty Empty)
		| value == element tree = tree
		| value > element tree = (Node (element tree) (left tree) (add value (right tree)))
		| otherwise = (Node (element tree) (add value (left tree)) (right tree))

treeToList :: (Ord a) => Tree a -> [a]
treeToList Empty = []
treeToList (Node element left right) = treeToList left ++ [element] ++ treeToList right

listToTree :: (Ord a) => [a] -> Tree a
listToTree [] = Empty
listToTree (x:xs) = add x $ listToTree xs

charValues :: Char -> [Int]
charValues x
	| x == 'a' = [1]
	| x == 'b' = [2]
	| x == 'c' || x == 'd' = [10]
	| x == 'e' = [1,11,21]
	| otherwise = []
stringValues :: String -> [Int]
stringValues [x] = charValues x
stringValues (x:xs) = [elements | elements <- treeToList $ listToTree [a+b |a <- charValues x, b <- stringValues xs]]

stringValue1 = map head . group . sort . foldl (\res x -> do
	 a <- x
	 b <- res
	 return (a+b)) [0] . map charValues 

stringValue2= map head . group . sort . foldM (\res x -> map (+res) x) 0 . map charValues 

-- #3 ready
maxHistory (x:xs) = maxHistoryIter (x:xs) x
	where 
		maxHistoryIter (x:[]) y = do
			if x >= y then do
				tell $ show x
				return x
			else do
				tell $ show y
				return y
		maxHistoryIter (x:xs) y = do
			if x >= y then do
				tell $ (show x ++ " -> ")
				maxHistoryIter xs x
			else do
				tell $ (show y ++ " -> ")
				maxHistoryIter xs y
-- При помощи foldM
maxHistoryFoldM (z:zs) = foldM (\y x -> do
	if x >= y then do
				tell $ (" -> " ++ show x)
				return x
			else do
				tell $ (" -> " ++ show y)
				return y) z (z:zs)





-- #5
-- Какую последовательность? Код ниже генерит длину рандомно в диапазоне, заданном пользователем.
randomSequenceGen = do
	putStrLn "Enter start number:"
	start <- readLn::(IO Int)
	putStrLn "Enter finish number:"
	finish <- readLn::(IO Int)
	--putStrLn $ start ++ finish
	randomNumber <- getStdRandom $ randomR (start, finish)
	appendFile "binary.txt" (show $ [1..randomNumber])


-- #6
-- bugged
{-
guessTheNumber = do
	putStrLn "guessTheNumber public alpha. Copyright a_kiselev, 2013."
	putStrLn "Answer should start with capital letter."
	asking 1 1000000
	

asking a b = 
	if a - b == 0 
		then putStrLn $ "Your number is " ++ show a
		else
			putStrLn $ "Is your number bigger than " ++ show middle ++ " ?"
			let answer = getLine in
			 case answer of 
				"Yes" -> asking middle b
				"No" -> asking a middle
				otherwise -> putStrLn "You are trying to trick me, stop it."
	where middle = round $ a / b
-}
