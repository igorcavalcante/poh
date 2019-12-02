-- value :: Expr -> Int
-- value (Val x) = x
-- value (Add x y) = value x + value y

data Expr = Val Int | Add Expr Expr | Mul Expr Expr
data Op = EVALA Expr | EVALM Expr| ADD Int | MUL Int
type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVALA y : c)
eval (Mul x y) c = eval x (EVALM y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALM y: c) n = eval y (MUL n : c)
exec (EVALA y: c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (m+n)
exec (MUL n : c) m = exec c (m*n)

value :: Expr -> Int
value e = eval e []

exp1 = (Add (Val 4) (Add (Val 2) (Val 3)))
exp2 = Mul (Val 4) (Val 2)

{- value Mul (Val 4) (Val 2)
ap value
eval Mul (Val 4) (Val 2) []
ap eval
eval (Val 4) [EVAL (Val 2)]
ap eval
exec [EVAL ] -}

{- value (Add (Val 4) (Add (Val 2) (Val 3)))
ap value
eval (Add (Val 4) (Add (Val 2) (Val 3))) []
ap eval
eval (Val 4) [EVAL (Add (Val 2) (Val 3))])
ap eval
exec [EVAL (Add (Val 2) (Val 3))]) 4
ap exec
eval (Add (Val 2) (Val 3)) [ADD 4]
ap eval
eval (Val 2) [(EVAL (Val 3) ), ADD 4]
app eval
exec [(EVAL (Val 3) ), ADD 4] 2
ap exec
eval (Val 3) [ADD 2, ADD 4]
ap eval
exec [ADD 2, ADD 4] 3
ap exec
exec [ADD 4] (2 + 3)
ap exec
exec [] (4 + (2 + 3))
ap exec
(4 + (2 + 3))
ap +
9 -}
{- value (Add (Val 2) (Val 3))
ap value
eval (Add (Val 2) (Val 3)) []
ap eval
eval (Val 2) [EVAL (Val 3)]
ap eval
exec [EVAL (Val 3)] 2
ap exec
eval (Val 3) [ADD 2]
ap eval
exec [ADD 2] 3
ap exec
exec [] 3 + 2
ap exec
3+2
ap +
5 -}
