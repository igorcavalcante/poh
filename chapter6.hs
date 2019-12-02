product' :: Num a => [a] -> a
product' [] = 1
product' xs = foldr (*) 1 xs

drop' :: Integral b => b -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs

fact :: Int -> Int
fact n   | n > 0 = n * fact (n - 1)
         | n < 0 = 0
         | otherwise = 1

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

expo :: Int -> Int -> Int
expo _ 0 = 1
expo n m = n * expo n (m - 1)

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x > y = euclid (x - y) y
           | x < y = euclid x (y - x) 

{- 
length [1,2,3,4]
applying length
1 + length [2,3,4]
applying length
1 + (1 + length [3,4])
applying length
1 + ( 1 + (1 length [4]))
applying length 
1 + (1 + (1 + (1 + length [])))
applying length
1 + (1 + (1 + (1 + 0)))
applying +
4
-}

{- 
drop 2 [1,2,3,4]
applying drop
drop 1 [2,3,4]
applying drop
drop 0 [3,4]
applying drop
[3,4]
-}

{-
init [1,2,3,4]
applying init
1 : init [2,3,4]
applying init
1 : (2: init [3,4])
applying init
1 : (2 : (3: init [4]))
applying init 
1 : (2 : (3: []))
applying sintax
[1,2,3]

and :: [Bool] -> Bool

-}

and' :: [Bool] -> Bool
and' [x] = x
and' (x:xs) = x && (and' xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:yss) = xs ++ (concat' yss)

repl :: Int -> a -> [a]
repl 0 x = []
repl n x = x : repl (n -1) x

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = xs !!! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) | a == x = True
              | otherwise = elem' a xs  

merge' :: Ord a => [a] -> [a] -> [a]
merge' [] ys = ys
merge' xs [] = xs
merge' (x:xs) (y:ys) 
    | x < y = merge' xs (x : y : ys) 
    |otherwise = merge' xs (y : merge' [x] ys)

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take half xs, drop half xs)
    where
        half = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort xs = merge' (msort xs') (msort ys) 
    where (xs', ys) = halve xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

take' :: Int -> [a] -> [a]
take' 0 xs = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

last' :: [a] -> a
last' (x:xs) = if null xs then x else last' xs
{- msort [5,4,3,2]
applying
merge' (msort[5,4]) (msort[3,2])
merge' (merge' (msort[5] msort [4])) -}

    

{- iOrd x [] = [x]
iOrd x (y:ys) | x < y = x : y : ys
              | otherwise = y : (iOrd x ys) -}