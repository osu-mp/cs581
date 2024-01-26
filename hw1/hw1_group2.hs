module Hw1_group2 where

---------------------------------------------------------------------------------------------------
-- Question 1 - Lists

-- definition of bag/multiset:
-- can be represented as list of pairs (x, n) where n indicates the number of occurrences of x in the multiset.
type Bag a = [(a,Int)]

-- (a) Define the function ins that inserts an element into a multiset
-- The class constraint ”Eq a =>” restricts the element type a to those types that allow the comparison of elements for equality with ==
ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x, 1)]  -- base case: empty bag; add the new element with count 1
ins x ((y, count):ys)
  | x == y    = (y, count + 1) : ys  -- item found, increment counter
  | otherwise = (y, count) : ins x ys  -- else, keep searching until we find target elemtn


-- (b) Define the function del that removes a single element from a multiset. Note that deleting 3 from {2, 3, 3, 4}
-- yields {2, 3, 4} whereas deleting 3 from {2, 3, 4} yields {2, 4}.
-- Delete function
del :: Eq a => a -> Bag a -> Bag a
del _ [] = []  -- base case: empty bag; nothing to delete
del x ((y, count):ys)
  | x == y = if count > 1 then (y, count - 1) : ys else ys  -- If more than one of the target items exissts in the bag then decrement count, else remove element
  | otherwise = (y, count) : del x ys  -- else continue searching in the rest of the bag
  
-- (c) Define a function bag that takes a list of values and produces a multiset representation.
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)

-- (d) Define a function subbag that determines whether or not its first argument bag is contained in the second.

itemBagCount :: Eq a => a -> Bag a -> Int
itemBagCount x bag = sum [n | (y, n) <- bag, y == x]

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True    -- empty bag is always a subbag of any bag
subbag _ [] = False   -- otherwise something will never be a subbag of an empy bag
subbag ((x, n):xs) ys = n <= itemBagCount x ys && subbag xs ys

 -- (e) Define a function isSet that tests whether a bag is actually a set, which is the case when each element occurs only once.
isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet ((_, count):xs) = count == 1 && isSet xs

-- (f) Define a function size that computes the number of elements contained in a bag.
size :: Bag a -> Int
size bag = sum [n | (_, n) <- bag]    -- sum up the counts of every item in the bag

---------------------------------------------------------------------------------------------------
-- Question 2 - Matrix

type Row    = [Int]
type Column = [Int]
type Matrix = [Row]

-- 2.a Vector Addition
vAdd :: Row -> Row -> Row
vAdd [] [] = []
vAdd (x:xs) (y:ys) = x + y : vAdd xs ys

vAddZipWith :: Row -> Row -> Row
vAddZipWith x y = zipWith (+) x y

-- 2.b Matrix Addition
mAdd :: Matrix -> Matrix -> Matrix
mAdd [] [] = []
mAdd x y = zipWith vAdd x y

-- 2.c Inner Product
iProd :: Row -> Column -> Int
iProd [] [] = 0
iProd (x:xs) (y:ys) = (x * y) + iProd xs ys

-- 2.d Outer Product
oProd :: Column -> Row -> Matrix
oProd [] _ = []
oProd (c:cs) ys = map (\x -> c * x ) ys : oProd cs ys

-- 2.e Size

mSize :: Matrix -> (Int, Int)
mSize [] = (0,0)
mSize (x:xs) = let (r, c) = mSize xs in (r + 1, foldl (\acc x -> acc + 1) 0 x)



---------------------------------------------------------------------------------------------------
-- Question 3 - Higher Order Funcs

-- hw1_3 part a
applyAll :: [b -> c] -> b -> [c]
applyAll funcs val = map (\func -> func val) funcs

-- hw1_3 part b
type Property a = [a -> Bool]
satisfies :: Property a -> a -> Bool
satisfies prop val = and $ applyAll prop val

-- hw1_3 part c
power :: ( a -> a) -> Int -> a -> a
power _ 0 x = x
power func num x = power func (num - 1) $ func x

-- hw1_3 part d
plus :: Int -> Int -> Int
plus a b
  | (a > 0) && (b > 0) = power succ a b
  | (a < 0) && (b > 0) = power pred (abs a) b
  | otherwise = power pred (abs b) a


times :: Int -> Int -> Int
times x y
  |( x < 0) == (y < 0) = power (plus (abs x)) (abs y) 0
  | otherwise = negate (power (plus (abs x)) (abs y) 0)
