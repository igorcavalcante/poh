import Data.List 
import Data.Char

comp xs f p = (map f . filter p) xs

comp2 = map (*2) . filter even

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = p x && all' p xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) = p x || any' p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
    | p x = xs
    | otherwise = dropWhile' p xs 

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y ) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x y -> if p x then x : y else y) []

dec2Int :: [Int] -> Int
dec2Int xs = foldl (\x a -> x * 10 + a) 0 xs

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \a -> \b -> f(a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(a,b) -> f a b

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

-- int2bin = unfold (==0) (`mod` 2) (`div` 2)

iterate' :: (a -> a) -> a -> [a]
iterate' f x = unfold (\_ -> False) id (f) x

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result'

result' xs = sort [(length x, head x) | x <- (group(sort xs))]

ballots :: [[String]]
ballots = [["Red", "Green"], ["Blue"], ["Green", "Red", "Blue"], ["Blue", "Green", "Red"], ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/=[])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/=x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
    [c] -> c
    (c: cs) -> winner' (elim c bs)

type Bit = Int

chop9 :: [Bit] -> [[Bit]]
chop9 xs = unfold (\x -> length x == 0) (take 9) (drop 9) xs

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold (\x -> length x == 0) (f . head) (tail)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

genParity :: [Bit] -> Bit
genParity xs = if (odd . count 1) xs then 1 else 0

encode :: String -> [Bit]
encode = concat . bits
    where
    bits xs = map (withParity . make8 . int2bin . ord) xs 
    withParity xs = genParity xs : xs

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chop9
        where
        checkParity (x:xs) = if x == (genParity xs) then xs else error "Parity bit error"

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = tail

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs

luhnDouble :: Int -> Int
luhnDouble x | y > 9 = y - 9
             | otherwise = y
             where y = x * 2

luhn :: [Int] -> Bool
luhn xs = ((sum . altMap (id) (luhnDouble) . reverse) xs) `mod` 10 == 0