-- File hutton/ch5.hs
-- Programming in Haskell - Graham Hutton - 2nd edition
-- Chapter 5 exercises

import Data.Char

-- exercise 1
-- Using a list comprehension, give an expression that calculates the sum
-- 1^2 + 2^2 _ ... + 100^2 of the first 100 integer squares.
squaresum :: Int -> Int
squaresum n = sum [ x ^ 2 | x <- [1..n] ]

-- exercise 2
-- Using a list comprehension, define a function grid :: Int -> Int -> [(Int,Int)]
-- that returns a coordinate grid of a given size.
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- exercise 3
-- Using a list comprehension and the function grid above, define a function
-- square :: Int -> [(Int,Int)] that returns a coordinate square of size n,
-- excluding the diagonal from (0,0) to (n,n).
square :: Int -> [(Int, Int)]
square n = [ (x,y) | (x,y) <- grid n n, x /= y ]

-- exercise 4
-- In a similar way to the function length, show how the library function
-- replicate :: Int -> a -> [a] that produces a list of identical elements
-- can be defined using a list comprehension.
replicate' :: Int -> a -> [a]
replicate' n a = [ a | _ <- [1..n] ]

-- exercise 5
-- Using a list comprehension with three generators, define a function
-- pyths :: Int -> [(Int,Int,Int)] that returns a list of all pythogorean
-- triples whose components are at most a given limit.
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]

-- exercise 6
-- Using a list comprehension and the function factors, define a function
-- perfects :: Int -> [Int] that returns a list of all perfect
-- numbers up to a given limit.
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum (factors x) - x == x ]

-- exercise 7
-- Show how the list comprehension [(x,7) | x <- [1,2], y <- [3,4]] with 2 generators
-- can be re-expressed using two comprehensions with single generators.
-- Hint: nest one comprehension within the other and make use of the library
-- function concat :: [[a]] -> [a].
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

ex7 = concat' [[(x,y) | x <- [1,2]] | y <- [3,4]]

-- exercise 8
-- Redefine the function postions using the function find
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [ v | (k',v) <- t, k == k' ]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- exercise 9
-- Show how a list comprehension can be used to define a function
-- scalarproduct :: [Int] -> [Int] -> Int that returns the scalar
-- product of two lists.
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x * y | (x, y) <- zip xs ys ]

-- exercise 10
-- Modify the Caesar cipher program to also handle upper-case letters.
let2int :: Char -> Int
let2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | ord c >= ord 'A' && ord c <= ord 'z' = int2let (mod (let2int c + n) 58)
  | otherwise                            = c
  
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
          
