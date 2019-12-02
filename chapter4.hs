halve :: [a] -> ([a], [a])
halve xs  | length xs `mod` 2 == 0 = (take midle xs, drop midle xs)
          | otherwise = ([],[])
          where midle = length xs `div` 2

third :: [a] -> Maybe a
-- third = if length > 2 then Just(head tail tail else Nothing
-- third xs | length xs > 2 = Just (xs !! 2)
--          | otherwise = Nothing
third (x:y:z:_) = Just z
third _ = Nothing

safetail :: [a] -> [a]
-- safetail xs = if null xs then xs else tail xs
-- safetail (x:xs) = xs
-- safetail _ = [] 
safetail xs | null xs = []
            | otherwise = tail xs

{- (||) :: Bool -> Bool -> Bool
False || _ = _ 
 _ || _ = True

_ || False = _ 
 _ || _ = True

False || False = False
 _ || _ = True

True || False = True
False || True = True
False || False = True -}

(&&&) :: Bool -> Bool -> Bool
-- fst &&& snd = if fst then fst else snd
-- (&&&) = \x y -> if x then y else x
(&&&) = \x y -> if x then if y then True else False else False

luhnDouble :: Int -> Int
luhnDouble x | y > 9 = y - 9
             | otherwise = y
             where y = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum ([luhnDouble a, b, luhnDouble c, d]) `mod` 10 == 0
{- 
1 7 8 4
2 7 16 4
2 7 7 4
16 + 4

2 14 16 4
2 5 7 4

18 -}
