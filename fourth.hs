-- ##### Second task ##### --
-- Geometry
data Point = Point {x :: Double, y :: Double}
    deriving(Eq, Ord)

instance Show Point where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")" 

data Domain = Rectangle {leftDown :: Point, rightUp :: Point} 
            | Circle {center :: Point, radius :: Double}
            | Intersection Domain Domain 
            | Union Domain Domain
            deriving(Show)

-- #1 ready, not tested
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

contains :: Point -> Domain -> Bool
contains point (Rectangle leftDown rightUp)
    | point == leftDown || point == rightUp = True
    | point > rightUp && point < leftDown = True
    | otherwise = False

contains point (Circle center radius)
    | point == center = True
    | distance center point <= radius = True
    | otherwise = False

contains point (Union domain1 domain2) =
    contains point domain1 || contains point domain2

contains point (Intersection domain1 domain2) =
    contains point domain1 && contains point domain2

-- #2
domainToPoint :: Domain -> Point
domainToPoint (Circle center _) = center
domainToPoint (Rectangle point _) = point
domainToPoint (Intersection domain1 domain2)
    | domainToPoint domain1 == domainToPoint domain2 = domainToPoint domain1
    | otherwise = undefined
domainToPoint (Union _ _) = undefined

isPoint :: Domain -> Bool

isPoint (Circle center radius) = radius == 0
isPoint (Rectangle leftDown rightUp) = leftDown == rightUp
isPoint (Union domain1 domain2)
    | isPoint domain1 == contains (domainToPoint domain1) domain2 = True
    | isPoint domain2 == contains (domainToPoint domain2) domain1 = True
    | otherwise = False
isPoint (Intersection domain1 domain2)
    | isPoint domain1 && isPoint domain2 = 
        domainToPoint domain1 == domainToPoint domain2
    | otherwise = False

onTheGridX :: Point -> Point -> Bool
onTheGridX (Point x1 y1) (Point x2 y2)
    | x1 == x2 = True
    | otherwise = False

onTheGridY :: Point -> Point -> Bool
onTheGridY (Point x1 y1) (Point x2 y2)
    | y1 == y2 = True
    | otherwise = False

within :: Domain -> Domain -> Bool
within (Rectangle a b) (Rectangle c d)
    | a < c && b > d = True
    | a > c && b < d = True
    | otherwise = False
within _ _ = undefined

-- rectangleUnion = 

isRectangular :: Domain -> Bool
isRectangular (Rectangle _ _) = True
isRectangular (Circle _ _) = False
isRectangular (Intersection domain1 domain2)
    | isRectangular domain1 && isRectangular domain2 = 
        within domain1 domain2
    | otherwise = False
isRectangular (Union (Rectangle a b) (Rectangle c d))
    | onTheGridY a c && (onTheGridX b c || onTheGridX a d) = True
    | onTheGridX a c && (onTheGridY b c || onTheGridX a d) = True
    | otherwise = False
{- isRectangular (Union domain1 domain2)
    |  -}  -- TODO: Union of two unions etc.

-- ##### Third task ##### -- 
data Prop = Var [Char] | And Prop Prop | Or Prop Prop | Not Prop

-- #1 ready, tested
vars :: Prop -> [[Char]]
vars (Var x) = [x]
vars (And x y) = vars x ++ vars y
vars (Or x y) = vars x ++ vars y
vars (Not x) = vars x

-- #2 ready, tested
truthValue :: Prop -> [([Char], Bool)] -> Bool
truthValue (Var x) list = 
    let value = filter (\(var, val) -> var == x) list in
        if value == [] then undefined
            else snd $ value !! 0
truthValue (And x y) list =
    truthValue x list && truthValue y list
truthValue (Or x y) list =
    truthValue x list || truthValue y list
truthValue (Not x) list =
    not $ truthValue x list

-- #3 (*) ready, tested

-- 1) Находим все названия переменных
-- 2) Генерим список всех возможных значений переменных, рекурсивно
-- 3) Вычисляем от каждого такого набора значения
-- 4) Вычисляем and от всего посчитанного

{-  [[("x", True)],[("x", False)]] -> [[("x", True),("y", True)],
                                       [("x", True),("y", False)],
                                        [("x", False),("y", True)],
                                        [("x", False),("y", False)]] -}

valuesGeneration :: [[Char]] -> [[([Char], Bool)]]
valuesGeneration [x] = [[(x, True)], [(x, False)]]
valuesGeneration (x:xs) = 
    (map (++ [(x, True)]) (valuesGeneration xs)) ++ (map (++ [(x, False)]) (valuesGeneration xs))

tautology :: Prop -> Bool
tautology formula = 
    let variables = vars formula
        valuesList = valuesGeneration variables
    in and $ map (truthValue formula) valuesList

-- ##### Fourth task ##### --
data Natural = One | Next Natural

instance Show Natural where
    show One = "Natural " ++ "1"
    show number = ("Natural " ++) $ show $ naturalToInteger number

readNatural :: String -> Natural
readNatural ('N':'a':'t':'u':'r':'a':'l':' ':x) = integerToNatural $ ((read x)::Integer) 

naturalToInteger :: Natural -> Integer
naturalToInteger One = 1
naturalToInteger (Next num) = 1 + naturalToInteger num

integerToNatural :: Integer -> Natural
integerToNatural num
    | num < 0 = undefined
    | num == 1 = One
    | otherwise = Next (integerToNatural (num-1))

multiply :: Natural -> Natural -> Natural
multiply num1 num2 = integerToNatural $ (naturalToInteger num1) * (naturalToInteger num2)


