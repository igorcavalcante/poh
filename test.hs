{-
double x = x + x
quadruple = double . double
-}
-- factorial of a positive integer
factorial n = product [1..n]
-- average
average ns = sum ns `div` length ns

a = b + c where {b = 1; c = 2}; d = a*2

x = (2^3)*4
x' = 2^3*4
y = (2*3)+(4*5)
y'=2*3+4*5
z = 2+(3*(4^5))
z'= 2+3*4^5

n = a `div` length xs
  where
  a = 10
  xs = [1,2,3,4,5]

last' xs = head (reverse xs) 
init' xs = reverse(tail (reverse xs))

init'' (x:[]) =  []
init'' (x:xs) = [x] ++ init xs

-- chapter 3
bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1],[3,4]]

add :: Int -> Int -> Int -> Int
add a b c = a+b+c

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a -> b) -> a -> b
apply fn a =  fn a

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,a) -> (a,a)
swap (a,b) = (b,a) 

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)
