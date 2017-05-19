-- File: hutton/ch6.hs
-- Programming in Haskell - Graham Hutton - 2nd edition
-- Chapter 6 exercises 

-- exercise 1
-- Modify the definition of the factorial function to prohibit negative
-- arguments by adding a guard to the recursive case.
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n - 1)
      | otherwise = 0

-- exercise 2
-- Define a recusive function sumdown :: Int -> Int that returns the sum
-- of the non-negative integers from a given value down to zero.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- exercise 3
-- Define the exponentiation operator ^ for non-negative integers using
-- the same pattern of recursion as the multiplication operator *
(^!) :: Int -> Int -> Int
m ^! 0 = 1
m ^! n | n > 0     = m * (m ^! (n - 1) )
       | otherwise = 1

-- exercise 4
-- Define a recursive function euclid :: Int -> Int -> Int that implements
-- Euclid's algorithm for calculating the greatest common divisor of two non-
-- negative integers: if the two numbers are equal, this number is the result;
-- otherwise, the smaller number is subtracted from the larger, and the same
-- process repeated.
euclid :: Int -> Int -> Int
euclid m n | m == n = m
           | m > n = euclid (m - n) n
           | otherwise = euclid m (n - m)

-- exercise 5
-- Show how length [1,2,3], drop 3 [1,2,3,4,5] and init [1,2,3] are evaluated.
{-
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

  length [1,2,3]
= { applying length }
  1 + length [2,3]
= { applying length }
  1 + ( 1 + length [3] )
= { applying length }
  1 + ( 1 + ( 1 + length [] )
= { applying length }
  1 + ( 1 + ( 1 + 0 ) )
= { applying + }
3
-}

{-
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = drop (n - 1) xs

  drop 3 [1,2,3,4,5]
= { applying drop }
  drop 2 [2,3,4,5]
= { applying drop }
  drop 1 [3,4,5]
= { applying drop }
  drop 0 [4,5]
= { applying drop }
  [4,5]
-}

{-
init :: [a] -> [a]
init [_] = []
init (x:xs) = x : init xs

  init [1,2,3]
= { applying init }
  1 : init [2,3]
= { applying init }
  1 : ( 2 : init [3] )
= { applying init }
  1 : ( 2 : [] )
= { applying :
  [1,2]
-}

-- exercise 6
-- Define the following library functions on lists using recursion.
-- exercise 6a
and' :: [Bool] -> Bool
and' [] = False
and' [x] = x == True
and' (x:xs) | x == True = and' xs
           | otherwise = False

-- exercise 6b
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

-- exercise 6c
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ replicate' (n - 1) x

-- exercise 6d
-- select the nth element of a list
(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)

-- exercise 6e
-- Decide if a value is an element of a list.
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) | x == y = True
              | otherwise = elem' x ys

-- exercise 7
-- Define a recursive function merge :: Ord a => [a] -> [a] that
-- merges two sorted lists to give a single sorted list.
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x == y    = x : y : merge xs ys
                    | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge ys (x:xs)

-- exercise 8
-- Using merge, define a function msort :: Ord a => [a] -> [a] that imple-
-- ments merge sort, in which the empty list and singleton lists are already
-- sorted, and any other list is sorted by merging together the two lists that
-- result from sorting the two halves of the list separately.
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2
  
msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs

-- exercise 9
-- Construct library functions that:
-- a. calculate the sum of a list of numbers;
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

-- b. take a given number of elements from the start of a list;
take' :: Int -> [a] -> [a]
take' 0 xs = []
take' n (x:xs) = [x] ++ take' (n - 1) xs

-- c. select the last element of a non-empty list.
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs
