class (Num a, Eq a, Ord a) => Vector a where
	(.+) :: [a] ->  [a] -> [a]
	(.+) = zipWith (+)
	opposite :: [a] -> [a]
	opposite = map negate
	(.-) :: [a] -> [a] -> [a]
	(.-) a b = a .+ (opposite b)
	(.*) :: a -> [a] -> [a]
	(.*) a = map (a*)
	
instance Vector Int
instance Vector Integer