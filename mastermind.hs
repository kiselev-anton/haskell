import Data.List (delete)
import System.Random (getStdRandom, random, randomRs, mkStdGen)
import Control.Monad (forever)
import System.Exit (exitSuccess)

main = do
  putStrLn $ "MasterMind on Haskell. 0-9. Give me length:"
  length <- readLn::(IO Int)

  putStrLn $ "Generating new row..."
  row <- rowGenerating length
  putStrLn $ "Done!"

  rowGuessing row


rowGuessing reference = forever $ do
  putStrLn $ "Enter your guess:"
  guess <- getLine

  let len = fromIntegral $ length reference
      (matchedPositions, nonmatchedChars, newReference) = rowMatching guess reference
      matchedDigits = rowChecking nonmatchedChars newReference
 
  if matchedPositions == 0 then do
    putStrLn $ show matchedDigits ++ " digits correctly guessed. Try again."
  else if matchedPositions == len then do
    putStrLn $ "You are right! The sequence was: " ++ reference
    exitSuccess
  else do 
    putStrLn $ show matchedPositions ++ " positions correctly matched. "
     ++ show matchedDigits ++ " digits correctly guessed. Try again."

rowChecking :: String -> String -> Integer

rowChecking test reference = 
  let checking (count, neededChars) char
        | elem char neededChars = (count + 1, delete char neededChars)
        | otherwise = (count, neededChars)
  in fst $ foldl checking (0, reference) test


rowMatching :: String -> String -> (Integer, String, String)

rowMatching test reference = 
  let matching (count, matches, list) (element1, element2)
        | element1 == element2 = (count+1, matches, delete element1 list)
        | otherwise = (count, element1:matches, list)
  in foldl matching (0::Integer, [], reference) $ zipWith (,) test reference  


rowGenerating length = do
  randomSeed <- getStdRandom $ random
  return $ concat $ map show $ take length $ randomRs (0::Integer, 9) $ mkStdGen randomSeed

