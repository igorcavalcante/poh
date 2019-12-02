data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

countLeaves (Leaf _) = 1
countLeaves (Node x y) = countLeaves x + countLeaves y

ts1 = (Node (Leaf 1) (Leaf 2))
ts2 = (Node (Leaf 3) (Leaf 4))
ts = Node ts1 ts2

tsu1 = (Node (Leaf 1) (Leaf 2))
tsu2 = (Node tsu21 tsu22)
tsu21 = (Node (Leaf 3) (Leaf 4))
tsu22 = (Node (Leaf 5) (Leaf 6))
tsu = Node tsu1 tsu2

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node x y) = diff <= 1 && balanced x && balanced y 
    where
        diff = abs (countLeaves x - countLeaves y) 

balance :: [a] -> Tree a
balance (x:[]) = Leaf x
balance xs = Node (balance x) (balance y)
    where
        (x,y) = halve xs

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take half xs, drop half xs)
    where
        half = length xs `div` 2

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
f :: Int -> Int
f _ = 1
g :: Int -> Int -> Int
g x y = x + y

folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

xp1 = Val 1
xp2 = Val 2
xp3 = Add xp1 xp2

{- instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    (Just x) == (Just y) = x == y
    _ == _  = False

instance Eq a => Eq [a] where
    [] == [] = True
    (x:xs) == (y:ys) = x == y && xs == ys -}

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equiv Prop Prop
    deriving Show

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Or (Var 'A') (Var 'B')

p4 :: Prop
p4 = Equiv (Var 'A') (Var 'A')

p5 :: Prop
p5 = Equiv p4 p3

type Assoc a b = [(a, b)]
type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q 

find :: Char -> Subst -> Bool
find x = snd . head . filter (\(a, b) -> a == x)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n -1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
