{-# LANGUAGE TypeFamilies, FlexibleContexts  #-}
-- #1

data (Num a, Show a) => TComplex a = Complex {real::a, imaginary::a}
	deriving(Eq)

instance (Show a, Num a) => Show (TComplex a) where
	show (Complex a b) = show a ++ " + " ++ show b ++ "i"

instance (Floating t, Num t, Show t) => Num (TComplex t) where
	(+) (Complex a b) (Complex c d) = (Complex (a + c) (b + d))
	(*) (Complex a b) (Complex c d) = (Complex (a*c - b*d) (a*c + b*d))
	(-) a b = a + negate b
	negate (Complex a b) = (Complex (-a) (-b))
	abs (Complex a b) = (Complex (sqrt $ (a^2 + b^2)) 0)
	signum = undefined
	fromInteger num = (Complex (fromInteger num) 0)

instance (Floating t, Num t, Show t) => Fractional (TComplex t) where
	(/) a b = a * (recip b)
	recip (Complex a b) = (Complex (a / (a^2+b^2)) (-b / (a^2+b^2)))
	fromRational num = (Complex (fromRational num) 0) 


-- #2
data (Eq a, Ord a) => Tree a = Empty | Node {element::a, left::Tree a, right::Tree a}
	deriving(Eq, Show)
-- Binary Search Tree
empty :: Tree a -> Bool
empty Empty = True
empty _ = False

member :: (Eq a, Ord a) => a -> Tree a -> Bool
member value tree
	| empty tree = False
	| element tree == value = True
	| value > element tree = member value $ right tree
	| otherwise = member value $ left tree

add :: (Eq a, Ord a) => a -> Tree a -> Tree a
add value tree
	| empty tree = (Node value Empty Empty)
	| value == element tree = tree
	| value > element tree = (Node (element tree) (left tree) (add value (right tree)))
	| otherwise = (Node (element tree) (add value (left tree)) (right tree))

quantity :: (Eq a, Ord a) => Tree a -> Integer
quantity tree
	| empty tree = 0
	| otherwise = 1 + quantity (left tree) + quantity (right tree)

sumTree :: (Num a, Eq a, Ord a) => Tree a -> a
sumTree tree
	| empty tree = 0
	| otherwise = element tree + (sumTree $ left tree) + (sumTree $ right tree)

productTree :: (Num a, Eq a, Ord a) => Tree a -> a
productTree tree
	| empty tree = 1
	| otherwise = element tree * (productTree $ left tree) * (productTree $ right tree)

remove :: (Eq a, Ord a) => a-> Tree a -> Tree a
remove value tree
	| empty tree = Empty
	| element tree == value = 
		if (empty $ right tree) && (empty $ left tree)
				then Empty
		else if empty $ right tree
			then Node (element $ left tree) (remove (element $ left tree) (left tree)) Empty
			else Node (element $ right tree) (left tree) (remove (element $ right tree) (right tree))
	| value > element tree = Node (element tree) (left tree) (remove value (right tree))
	| otherwise = Node (element tree) (remove value (left tree)) (right tree)