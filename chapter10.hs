import Data.Char
import System.IO

putStr' :: String -> IO ()
putStr' xs =  sequence_ [putChar x | x <- xs]

type Board = [Int]

putRow :: Int -> Int -> IO()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard xs = sequence_ [putRow x y | (x, y) <- zip [1..] xs]

putBoardRec :: Board -> IO ()
putBoardRec [] = return ()
putBoardRec xs = rec 1 xs 
                    where
                        rec n (y:ys) = do putRow n y
                                          rec (n+1) ys
                                          return ()
                        rec n [] = return()

adder :: IO() 
adder = do putStr "How many numbers? "
           n <- getChar
           putStrLn "" 
           ys <- sequence [getChar | _ <- [1..digitToInt(n)]]
           zs <- (return . sum . map digitToInt) ys
           putStrLn (show zs) 

-- add :: Int -> IO (Int)
-- add n = do ys <- sequence [getChar, getChar]
--            zs <- map (digitToInt) ys         
--            return (zs)

-- \DEL
-- \b

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True     
           return x

getLine' :: IO String
getLine' = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else if x == '\DEL' then
                do putChar '\b'
                   putChar ' '
                   putChar '\b'
                   xs <- getLine'
                   return (x:xs) 
              else 
                do putChar '-'
                   ys <- getLine'
                   if not (null ys) && head ys == '\DEL' then
                     return (tail ys)
                   else
                     return (x:ys)