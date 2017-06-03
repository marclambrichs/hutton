-- File hutton/ch4.hs
-- Programming in Haskell - Graham Hutton - 2nd edition
-- Chapter 4 exercises

-- execrcise 1
-- Using library functions, define a function halve that splits
-- an even-lengthed list into two halves.
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

-- exercise 2
-- Define a function third that returns the third element
-- in a list that contains at least this many elements.

-- exercise 2a
-- using head and tail
-- third xs = head ( tail ( tail xs ) ) -- or
third :: [a] -> a
third = head . tail . tail

-- exercise 2b
-- using list indexing
third' :: [a] -> a
third' xs = xs !! 2

-- exercise 2c
-- using pattern matching
third'' :: [a] -> a
third'' (_:(_:(x:_))) = x

-- exerise 3
-- Consider a function safetail that behaves in the same way
-- as tail except that it maps the empty list to itself rather
-- than producing an error. Using tail and the function null :: [a] -> Bool
-- that decides if a list is empty or not, define safetail using

-- exercise 3a
-- using a conditional expression
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- exercise 3b
-- using guarded equations
safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

-- exercise 3c
-- using pattern matching
safetail'' :: [a] -> [a]
safetail'' []     = []
safetail'' (_:xs) = xs

-- exercise 4
-- In a similar way to && in section 4.4, show how the disjunction
-- operator || can be defined in 4 different ways using pattern matching.

or2 :: Bool -> Bool -> Bool
True `or2` True   = True
True `or2` False  = True
False `or2` True   = True
False `or2` False  = False


or3 :: Bool -> Bool -> Bool
False `or3` False = False
_     `or3` _     = True

or4 :: Bool -> Bool -> Bool
False `or4` b = b
_     `or4` _ = True

or5 :: Bool -> Bool -> Bool
b `or5` c | b == c     = b
        | otherwise = True

-- exercise 5
-- Without using any other library functions or operators, show how the
-- meaning of the following pattern matching definition for logical
-- conjunction && can be formalised using conditional expressions.
-- True && True = True
-- _    && _    = False
and2 :: Bool -> Bool -> Bool
and2 b c = if b then (if c then True else False) else False

-- exercise 6
-- Do the same for the following alternative definition, and note the
-- difference in the number of conditional expressions that are required.
-- True &&  b = b
-- False && _ = False
and3 :: Bool -> Bool -> Bool
and3 a b = if a then b else False

-- exercise 7
-- Show how the meaning of the following curried function definition can be
-- formalised in terms of lambda expressions.
-- mult :: Int -> Int -> Int -> Int
-- mult x y z = x * y * z
mult' :: Int -> Int -> Int -> Int
mult' = \x -> \y -> \z -> x * y * z

-- exercise 8
-- The Luhn algorithm is used to check bank card numbers for simple errors
-- such as mistyping a digit, and proceeds as follows.

-- Define a function luhnDouble that doubles a digit and subtracts 9 if
-- the result is greater than 9.
luhnDouble :: Int -> Int
luhnDouble x | 2 * x <= 9 = 2 * x
             | otherwise  = (2 * x) - 9

-- Using luhnDouble and the integer remainder function mod, define a function
-- luhn that decides if a four-digit bank card number is valid.
luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w + x +luhnDouble y + z) `mod` 10 == 0
