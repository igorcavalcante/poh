data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero x = x
add (Succ n) t = Succ(add n t) 

mul :: Nat -> Nat -> Nat
mul x Zero = Zero
mul x (Succ z) = add x (mul x z) 

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) | x == y    = True
                      | x > y     = occurs x r
                      | otherwise = occurs x l

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = compare x y == EQ
occurs' x (Node l y r) = f z
  where
  z = compare x y
  f EQ = True
  f GT = occurs' x r
  f LT = occurs' x l

exp :: Nat -> Nat -> Nat
exp x Zero = Succ(Zero)
exp x (Succ z) =  x (mul x z) 