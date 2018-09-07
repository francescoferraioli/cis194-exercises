module Week04.Soln
  ( fun1
  , fun2
  , fun1'
  , fun2'
  , Tree(..)
  , foldTree
  , showTree
  , printTree
  , xor
  , map'
  , myFoldl
  , cartProd
  , sieveSundaram
  ) where

---------------------------  Exercise 1

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = error "Week04.Soln#foldTree not implemented"



---------------------------  Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr buildTree Leaf

buildTree :: a -> Tree a -> Tree a
buildTree x Leaf = Node 0 Leaf x Leaf
buildTree x (Node _ l y r) 
 | getHeight l < getHeight r = node y (buildTree x l) r
 | otherwise = node y (buildTree x r) l

node :: a -> Tree a -> Tree a -> Tree a
node y a b
  | getHeight a < getHeight b = Node (getHeight b + 1) b y a 
  | otherwise = Node (getHeight a + 1) a y b 

getHeight :: Tree a -> Integer
getHeight Leaf = -1
getHeight (Node h _ _ _) = h

showTree :: Show a => Tree a -> String
showTree Leaf = ""
showTree n@(Node s _ _ _) = go s n
  where
  go _ (Leaf) = ""
  go i (Node h l c r) = go (i-1) l ++
    replicate (4*fromIntegral i) ' ' ++ show c ++ "-" ++ show h ++ "\n" ++ go (i-1) r

-- will print a tree in ghci, root node will be the rightmost in the printed structure
-- nodes will be printed as [value]-[height]
printTree :: Show a => Tree a -> IO ()
printTree t = putStrLn $ showTree t

---------------------------  Exercise 3

xor :: [Bool] -> Bool
xor x = foldr (\_ b -> not b) False (filter (==True) x)

-- impl using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ a b -> f a : b) []

-- impl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl = error "Week04.Soln#myFoldl not implemented"

---------------------------  Exercise 4
-- See: https://en.wikipedia.org/wiki/Sieve_of_Sundaram
-- cartProd provided for you to use in your solution

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram = error "Week04.Soln#sieveSundaram not implemented"
