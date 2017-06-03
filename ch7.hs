-- File hutton/ch7.hs
-- Programming in Haskell - Graham Hutton - 2nd edition
-- Chapter 7 exercises

-- exercise 1
-- Show how the list comprehension [f x | x <- xs, px] can be re-expressed
-- using the higher-order functions map and filter.
-- map f (filter p x)

-- exercise 2
-- exercise 2a
-- Decide if all elements of a list satisfy a predicate:
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

-- exercise 2b
-- Decide if any element of a list satisfies a predicate
any' :: (a-> Bool) -> [a] -> Bool
any' p = or . map p

-- exercise 2c
-- Select elements from a list while they satisfy a predicate
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x        = x : takeWhile' p xs
                    | otherwise  = []

-- exercise 2d
-- Remove elements from a list while they satisfy a predicate
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs

-- exercise 3
-- Redefine the functions map f and filter p using foldr.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- exercise 4
-- Using foldl, define a function dec2int :: [Int] -> Int that converts a
-- decimal number into an integer.
dec2int :: [Int] -> Int
dec2int  = foldl (\x y -> y + 10 * x) 0

-- exercise 5
-- Define the higher-order library function curry that converts a function on
-- pairs into a curried function, and, conversely, the function uncurry that
-- converts a curried function with two arguments into a function on pairs.
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f(x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y

-- exercise 6
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)


int2bin = unfold (==0) (`mod` 2) (`div` 2)

--chop8 :: [Bit] -> [[Bit]]
--chop8 = unfold (\xs -> length xs == 8)
--
--map' :: (a -> b) -> [a] -> [b]
--map' f xs

-- exercise 9
-- Define a function altMap that alternatively applies its two argument functions
-- to successive elements in a list, in turn about order.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs

-- exercise 10
-- Using altMap, define a function luhn that implements the Luhn algorithm
-- from the exercises in chapter 4 for bank card numbers of any length.
luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sum . (altMap (id) (\x -> (2 * x) `mod` 9)) . reverse
