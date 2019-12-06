import           Data.List
main :: IO ()

data Op = Add | Sub | Mul | Div | Exp
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = y > 0 && x `mod` y == 0
valid Exp x y = x > 1 && y > 0 && x ^ y > 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                        brak (Val n) = show n
                        brak e       = "(" ++ show e ++ ")"


values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

{- subs [2,3]
ap subs
((subs [3]) ++ map (2:) (subs [3]))
ap subs
(subs []) ++ map (3:) (subs []) ++ map (2:) (subs [3])
ap subs
[[]] ++ map (3:) [[]] ++ map (2:) (subs [3])
ap ++
[[], [3]]  ++ map (2:) [[], [3]]
ap map
[[], [3]]  ++ [[2], [2, 3]]
ap ++
[[], [3], [2], [2, 3]]
-}

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

{- interleave 1 [2,3]
ap inter
(1:2:3) : map (2:) (interleave 1 [3])
ap interleave
(1:2:3) : map (2:) ((1:3:[]) : map (3:) (interleave 1 []))
ap interleave
(1:2:3) : map (2:) ((1:3:[]) : map (3:) ([[1]]))
ap map
(1:2:3) : map (2:) [[1,3],[3,1]])
ap map
[[1,2,3], [2,1,3],[2,3,1]]) -}

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

{- perms [1,2]
ap perms
concat (map (interleave 1) (perms [2]))
ap perms
concat (map (interleave 1) (concat (map (interleave 2 perms []))))
ap perms
concat (map (interleave 1) (concat (map (interleave 2 []))))
ap map
concat (map (interleave 1) (concat ([[2]]))))
ap concat
concat (map (interleave 1) [[2]])
ap map

 -}

choices :: [a] -> [[a]]
-- choices = concat . map perms . subs
choices xs = [ps | ss <- subs xs, ps <- perms ss]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l        <- exprs ls,
                r        <- exprs rs,
                e        <- combine l r]

xprs :: [a] -> [(a,a)]
xprs ns = concat [[l, r] | (ls, rs) <- split ns,
                    l        <- xprs ls,
                    r        <- xprs rs]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

xpx = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))
ns = [1 :: Int,3,7,10,25,50]

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice _ [] = False
isChoice [] _ = True
isChoice (x:xs) ys | isIn = (isChoice xs . removeFirst x) ys
                   | otherwise = False
                       where isIn = (not . null . filter (\i -> i == x)) ys

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst y [] = []
removeFirst y (x:xs) | x == y = xs
                     | otherwise = [x] ++ removeFirst y xs

s' :: [Int] -> [Expr]
s' ns = [e | ns' <- choices ns, e <- exprs ns', eval e /= []]
rs' = s' [1::Int, 3,7,10,25,50]
-- main = print (length rs')

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n | length ss > 0 = ss
                | otherwise = (take 10 . map (snd) . sorted) [res x| x <- s' ns, eval x /= []]
                    where ss = solutions ns n
                          sorted rs = sortBy(\(x, y) (a, b) -> compare x a) rs
                          res e = (abs (head (eval e) - n), e)

compare' :: Expr -> Expr -> Ordering
compare' x y = compare (countValues x) (countValues y)

countValues :: Expr -> Int
countValues (Val _)     = 1
countValues (App _ l r) = countValues l + countValues r


main = print ( sortBy (compare') (solutions ns 765))
