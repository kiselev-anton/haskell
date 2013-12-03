import System.Random
import Data.List
import System.IO
import Data.Char
import Data.Ord
{- 
##### STATUS #####

#1 done
#2 done
#3 ready
#4 done
#5 done
#6 done

#####        #####
 -}


-- #1
-- done
sortInFile = do
	file <- readFile "input.txt"
	-- числа идут через пробелы 
	let sorting = sortBy (comparing negate) . map (read::(String -> Integer)) . words -- . filter (/='\n')
	let sortedList = unwords $ map show $ sorting file
	writeFile "output.txt" $ sortedList ++ "\n"

-- #2
-- done
symbolsInFile fileInput fileOutput = do
	file <- readFile fileInput
	let quantity char = length $ filter (== char) file
	let chars = [chr 32 .. chr 127]
	writeFile fileOutput $ (show $ zipWith (,) chars $ map quantity chars) ++ "\n"

-- SORTBY

-- #3
-- ready
-- case insensitive
fileWords fileInput fileOutput = do
	file <- readFile fileInput
	writeFile fileOutput $ (unwords $ nubBy (\x y -> map toLower x == map toLower y) $ sortBy (comparing $ map toLower) $ words file) ++ "\n"

-- #4
-- done
linesWithWord word fileInput fileOutput = do
	file <- readFile fileInput
	let linesOfFile = lines file
	let sortedLines = filter (isInfixOf word) linesOfFile
	writeFile fileOutput $ unlines sortedLines


-- #5
-- done
randomSequenceGen = do
	-- Считываем начальные значения
	putStrLn "Enter start number:"
	start <- readLn::(IO Int)
	putStrLn "Enter finish number:"
	finish <- readLn::(IO Int)
	-- ,Берём случайное число из интервала
	randomNumber <- getStdRandom $ randomR (start, finish)
	-- Засовываем в файл случайные числа из интервала, длина списка с которыми случайна.
	-- Список генерируется стандартным генератором, созданным
	-- при помощи случайного числа, полученного ранее.
	putStrLn (show $ take randomNumber $ randomRs (start,finish) $ mkStdGen randomNumber)
	--appendFile "binary.txt" (show $ take randomNumber $ randomRs (start,finish) $ mkStdGen randomNumber)

-- #6
-- done
guessTheNumber = do
	putStrLn "guessTheNumber public alpha. Copyright a_kiselev, 2013."
	putStrLn "Answer should start with capital letter."
	putStrLn "Possible answers: Yes, No, Equal"
	asking 1 1000000
	
asking :: Integer -> Integer -> IO () 
asking a b = 
	if b - a == 1 || b - a == 0
		then putStrLn $ "Your number is " ++ show a
		else do
			putStrLn $ "Is your number bigger than or equal to " ++ show middle ++ " ?"
			answer <- getLine
			case answer of 
				"Yes" -> asking middle b
				"No" -> asking a middle
				"Equal" -> putStrLn $ "Your number is " ++ show middle
				otherwise -> do
					putStrLn "You are trying to trick me, stop it."
					asking a b

	where middle = a + quot (b - a) 2