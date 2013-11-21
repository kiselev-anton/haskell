import Data.Maybe
import Control.Applicative
import Control.Monad.Writer
import Data.List

-- #1 ready
data (Num t, Show t, Eq t) => TLine t = Line {a::t, b::t, c::t}
    deriving(Show, Eq)
data (Num t, Show t, Eq t) => TPoint t = Point {x::t, y::t}
    deriving(Show, Eq)

intersection :: (Fractional a, Show a, Eq a) => TLine a -> TLine a -> Maybe (TPoint a)
intersection (Line a1 b1 c1) (Line a2 b2 c2) = 
    if a1 / a2 == b1 / b2 
        then Nothing
        else Just (Point (-(c1*b2 - c2*b1) / det) 
                         (-(a1*c2 - a2*c1) / det))
            where det = a1*b2 - a2*b1

-- #2 ready
{-
intersections :: (Fractional a, Show a, Eq a) => [TLine a] -> [TPoint a]

intersections [] = []
intersections (line:lines) =
 foldl (\list line2 -> let test = intersection line line2 in
                            if test == Nothing 
                                then list
                                else fromJust test : list)
 [] lines ++ intersections lines
-}

intersections :: (Fractional a, Show a, Eq a) => [TLine a] -> [TPoint a]
intersections lines = nub [fromJust $ intersection line1 line2 | line1 <- lines, line2 <- lines, intersection line1 line2 /= Nothing]

-- #3 ready
-- Applicative functors
cartesianProduct1 :: [a] -> [a] -> [[a]]
cartesianProduct1 x y = (\x y -> [x, y]) <$> x <*> y

-- List comprehension
cartesianProduct2 :: [a] -> [a] -> [[a]]
cartesianProduct2 xs ys = [[x,y] | x <- xs, y <- ys]

-- Bicycle
cartesianProduct3 :: [a] -> [a] -> [[a]]
cartesianProduct3 [] xs = []
cartesianProduct3 (x:xs) ys = map (\y -> [x, y]) ys ++ cartesianProduct3 xs ys

-- Monads
cartesianProduct4 :: [a] -> [a] -> [[a]]
cartesianProduct4 list1 list2 = list1 >>= (\a -> map (\x -> [a,x]) list2)

--Monads2
cartesianProduct5 :: [a] -> [a] -> [[a]]
cartesianProduct5 list1 list2 = do
    a <- list1
    b <- list2
    return [a,b]

-- #4 bugged
-- a=1, b=2, c,d=10, e=1,11,21
-- String = [Char]

-- TODO: fix repetitions

charValues :: Char -> [Int]
charValues x
    | x == 'a' = [1]
    | x == 'b' = [2]
    | x == 'c' || x == 'd' = [10]
    | x == 'e' = [1,11,21]
    | otherwise = []
stringValues :: String -> [Int]
stringValues [x] = charValues x
stringValues string = 
    foldl1 (\list1 list2 -> map sum $ cartesianProduct1 list1 list2) $ map charValues string



-- #5 ready
--maxWithHistory :: 
maxWithHistory list = 
    let maxHistory = maxWithHistoryTemp list
        maximum = fst maxHistory
        history = concat $ snd maxHistory 
        historyLength = length history
        historyFixed = take (historyLength - 4) history
    in (maximum, historyFixed)

maxWithHistoryTemp (x:xs) = runWriter $ foldl 
    (\wr b ->   if b > (a wr) 
        then writer(b, chain wr ++ [show b ++ " -> "]) 
        else writer((a wr), chain wr ++ [show (a wr) ++ " -> "])) 
    (writer (x, [show x ++ " -> "])) xs
        where 
            a wr = fst $ runWriter wr
            chain wr = snd $ runWriter wr 

-- Суть алгоритма:
-- 1) Проходимся фолдом по данному списку, начинаем фолд с writer-а, который создаём на основе первого элемента
-- 2) Затем, вытаскивая элемент из него, сверяем его с новым. Создаём на основе новый writer, выписывая в него историю.
-- 3) Все последовательные действия склеиваем и убираем лишнее с конца.

-- P.S. Код не очень хороший и суть монады writer не используется, используется лишь та её часть, которая похожа на пару.