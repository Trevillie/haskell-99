-- 99 questions
-- 2015.1.4 .. Trevillie

module Problem_Set where

import Data.List
import Control.Applicative
import Control.Monad
import Data.Function ( on)
import Data.Char     ( isAlphaNum, isLetter)

-- (*) Find the last element of a list.
solution_1 :: [a] -> a
solution_1 [] = error "Empty List"
solution_1 [x] = x
solution_1 (_:xs) = solution_1 xs


-- (*) Find the last but one element of a list.
solution_2 :: [a] -> a
solution_2 [] = error "Empty List"
solution_2 [x] = error "Singleton"
solution_2 [x, y] = x
solution_2 (x:xs) = solution_2 xs

solution_2' :: [a] -> a
solution_2' = last . init

-- (*) Find the K'th element of a list. The first element in the list is number 1.
solution_3 :: [a] -> Int -> a
solution_3 [] _ = error "Index Out Of Bound"
solution_3 (x:xs) 1 = x
solution_3 (x:xs) n
  | n <= 0 = error "Negative Index Or Zero"
  | otherwise = solution_3 xs (n - 1)


-- (*) Find the number of elements of a list.
solution_4 :: [a] -> Int
solution_4 [] = 0
solution_4 (_:xs) = 1 + solution_4 xs

solution_4' :: [a] -> Int
solution_4' = foldr (\_ y -> y + 1) 0 


-- (*) Reverse a list.
solution_5 :: [a] -> [a]
solution_5 [] = []
solution_5 [x] = [x]
solution_5 (x:xs) = (solution_5 xs) ++ [x]

solution_5' :: [a] -> [a]
solution_5' = foldl (\xs x -> x:xs) []


-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
solution_6 :: Eq a => [a] -> Bool
solution_6 xs = and $ map (\(x, y) -> x == y) $ zip xs (reverse xs) -- use zipWith

solution_6' :: Eq a => [a] -> Bool
solution_6' xs = xs == (reverse xs)


-- (**) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
solution_7 :: NestedList a -> [a]
solution_7 (List []) = []
solution_7 (Elem x) = [x]
solution_7 (List (x:xs)) = solution_7 x ++ solution_7 (List xs) -- concatMap is better


-- (**) Eliminate consecutive duplicates of list elements.
solution_8 :: Eq a => [a] -> [a]
solution_8 [] = []      --
solution_8 [x] = [x]    -- write solution_8 xs = xs after will do the boundary in one line
solution_8 (x:y:zs)
  | x == y = solution_8 (y:zs)
  | otherwise = x:solution_8 (y:zs) 

solution_8' :: Eq a => [a] -> [a]
solution_8' = map head . group
  
  
-- (**) Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists.
-- group
solution_9 :: Eq a => [a] -> [[a]]
solution_9 l = acc_group [] l
  where acc_group [] [x] = [[x]]
        acc_group gs [x] = [x:gs]
        acc_group _ []   = []
        acc_group gs (x:y:zs)
          | x == y = acc_group (x:gs) (y:zs)
          | otherwise = (x:gs) :  acc_group [] (y:zs)
	
solution_9' :: Eq a => [a] -> [[a]]
solution_9' [] = []
solution_9' (x:xs) = let (fp, lp) = span (== x) (x:xs)
                   in fp : solution_9' lp


-- (*) Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
solution_10 :: Eq a => [a] -> [(Int, a)]
solution_10 = map (\(x:xs) -> (1 + length xs, x)) . solution_9'

solution_10' :: Eq a => [a] -> [(Int, a)]
solution_10' l = [(length xs, head xs) | xs <- solution_9' l]
-- list comprehension is cool


-- (*) Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates,
-- it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data RLE a = Multiple Int a | Single a
  deriving (Show)
  
solution_11 :: Eq a => [a] -> [RLE a]
solution_11 = map transcode . solution_10 
  where transcode (1, x) = Single x
        transcode (n, x) = Multiple n x

solution_11' :: Eq a => [a] -> [RLE a]
solution_11' xs = [if (length x) == 1 then Single (head x) else Multiple (length x) (head x) | x <- group xs]
		

-- (**) Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
solution_12 :: [RLE a] -> [a]
solution_12 = foldr decode []
  where decode (Single x) xs = x:xs
        decode (Multiple n x) xs = (take n $ repeat x) ++ xs

solution_12' :: [RLE a] -> [a]
solution_12' = concatMap f
  where f (Single x) = [x]
        f (Multiple n x) = take n $ repeat x

		
-- (**) Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
helper_13 :: Eq a => [a] -> [(Int, a)]
helper_13 = foldl f []
  where f [] x = [(1, x)]
        f ((ny, y):zs) x
          | x == y = (1+ny, y):zs
          | otherwise = (1, x):(ny, y):zs

solution_13 :: Eq a => [a] -> [RLE a]
solution_13 = reverse . map encodeHelper . helper_13
  where encodeHelper (1,x) = Single x
        encodeHelper (n,x) = Multiple n x


-- (*) Duplicate the elements of a list.
solution_14 :: [a] -> [a]
solution_14 = concatMap (replicate 2)

solution_14' :: [a] -> [a]
solution_14' = flip (>>=) (\x -> [x,x])    -- Monad BIG LAW GOOD


-- (**) Replicate the elements of a list a given number of times.
solution_15 :: [a] -> Int -> [a]
solution_15 l n = concatMap (replicate n) l


-- (**) Drop every N'th element from a list.
solution_16 :: [a] -> Int -> [a]
solution_16 xs n = concat $ zipWith (\x y -> if x then [y] else []) (cycle $ replicate (n-1) True ++ [False]) xs

solution_16' :: [a] -> Int -> [a]
solution_16' xs n = dropHelper xs 1
  where dropHelper [] _ = []
        dropHelper (x:xs) 1 = dropHelper xs n 
        dropHelper (x:xs) k = x : dropHelper xs (k-1)


-- (*) Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
solution_17 :: [a] -> Int -> ([a], [a])
solution_17 l n = helper [] l n
  where helper xs zs 0 = (reverse xs, zs)
        helper xs [] n = error "Index Out Of Bound"
        helper xs (y:zs) n = helper (y:xs) zs (n-1)

solution_17' :: [a] -> Int -> ([a], [a])
solution_17' [] _ = ([], [])
solution_17' l@(x:xs) n
  | n <= 0 = ([], l)
  | otherwise = (x:ys, zs)
  where (ys, zs) = solution_17' xs (n-1)       -- use pattern matching to split the two parts
                                               -- this IS smart

											   
-- (**) Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element
-- of the original list (both limits included). Start counting the elements with 1.
solution_18 :: [a] -> Int -> Int -> [a]
solution_18 l m n = take (n+1 - m) . drop (m-1) $ l


----------------------------------------------------------------------------
-- 1-18 == 1.6

-- (**) Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
-- I DO think my solution here is better then those on https://www.haskell.org/haskellwiki/99_questions/Solutions/19
solution_19 :: [a] -> Int -> [a]
solution_19 xs n = let n' = if n > 0 then n else length xs + n
                   in concat $ [drop n', take n'] <*> [xs]


-- (*) Remove the K'th element from a list.
-- *Main> removeAt 2 "abcd"
-- ('b',"acd")
solution_20 :: Int -> [a] -> (a, [a])
solution_20 n xs = (xs !! (n-1), concat $ [take (n-1), drop n] <*> [xs])


-- Insert an element at a given position into a list.
solution_21 :: a -> [a] -> Int -> [a]
solution_21 x xs n = take (n-1) xs ++ (x:drop (n-1) xs)


-- Create a list containing all integers within a given range.
solution_22 :: Int -> Int -> [Int]
solution_22 x y = [x..y] 


-- Extract a given number of randomly selected elements from a list.
-- Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
-- solution_23 :: [a] -> Int

----
--  |
--  | 23, 24, 25
--  |
----

------------------
-- Good Problem --
------------------
-- (**) Generate the combinations of K distinct objects chosen from the N elements of a list
solution_26 :: Int -> [a] -> [[a]]
solution_26 0 _ = [[]]
solution_26 n xs = [y:ys' | y:ys <- tails xs, ys' <- solution_26 (n-1) ys]    -- move recursively in list comprehension

solution_26' :: Int -> [a] -> [[a]]
solution_26' 0 _ = return []
solution_26' n xs = do y:xs' <- tails xs
                       ys <- solution_26' (n-1) xs'
                       return (y:ys)
					   
------------------
-- Good Problem --
------------------
-- Group the elements of a set into disjoint subsets.
helper_27 :: Int -> [a] -> [([a], [a])]
helper_27 0 xs = [([], xs)]
helper_27 _ [] = []
helper_27 n (x:xs) = la ++ lb
  where la = [(x:ys, zs) | (ys, zs) <- helper_27 (n-1) xs]
        lb = [(ys, x:zs) | (ys, zs) <- helper_27  n    xs]      -- do not even do a pattern matching when xs is []

solution_27 :: [Int] -> [a] -> [[[a]]]
solution_27 [] _ = [[]]
solution_27 num@(n:ns) names
  | sum num /= length names = error "Invalid Partition Plan"
  | any (<= 0) num          = error "Invalid Partition Plan"
  | otherwise               = [xs:ys' | (xs, ys) <- helper_27 n names, ys' <- solution_27 ns ys]

  
----------------------------------------------------------------------------
-- 1-22, 26, 27 == 1.7


-- Sorting a list of lists according to length of sublists
-- Again, we suppose that a list contains elements that are lists themselves.
-- But this time the objective is to sort the elements of this list according to their length frequency;
-- i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first,
-- others with a more frequent length come later.
solution_28 :: [[a]] -> [[a]]
solution_28 = concat . sort' . group' . sort'
  where sort'  = sortBy (compare `on` length)
        group' = groupBy (\x y -> length x == length y)


-- problem 29 is a gap
-- problem 30 is a gap


-- (**) Determine whether a given integer number is prime.		
solution_31 :: Integral a => a -> Bool
solution_31 n = and $ (n > 1) : map ((/= 0) . mod n) [x |x <- [2 .. (n-1)], x * x <= n]  -- this is slow
                                                                                         -- 9999991 runs 7.98s roughly on :set +s

solution_31' :: Integral a => a -> Bool
solution_31' n | n < 4 = n /= 1                                                          -- | is the functional dependency key word, -> "when"
solution_31' n = all ((/=0) . mod n) $ takeWhile (<= m) candidates 
  where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])
        m = floor . sqrt $ fromIntegral n                                                -- this one finishes in no time
                                                                                         -- why so fast ...


-- (**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
solution_32 :: Integral a => a -> a -> a
solution_32 x y
  | y == 0    = abs x
  | otherwise = solution_32 y $ mod x y


-- (*) Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.
solution_33 :: Integral a => a -> a -> Bool
solution_33 x y = gcd x y == 1


-- (**) Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
solution_34 :: Integral a => a -> Int
solution_34 1 = 1
solution_34 n = length [x | x <- [1 .. n-1], solution_33 x n]            -- this one is slow but I do not need a faster one ...


----------------------------------------------------------------------------
-- 1-22, 26-34 == 1.8


-- well, i skipped some problems here


-- Tree Defination
data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

-- Binary Tree Part
-- (**) Construct completely balanced binary trees
solution_55 :: Int -> [Tree Char]
solution_55 0 = [Empty]
solution_55 x = [Branch 'x' sub_1 sub_2 | l <- [base .. (base+addon)],
                                          sub_1 <- solution_55 l,
                                          sub_2 <- solution_55 (x - l - 1)]
  where x'    = x - 1
        base  = div x' 2
        addon = mod x' 2


-- (**) Symmetric binary trees
equal_tree :: Tree a -> Tree a -> Bool
equal_tree Empty            Empty            = True
equal_tree (Branch _ x1 x2) (Branch _ y1 y2) = (equal_tree x1 y2) && (equal_tree x2 y1)
equal_tree _                _                = False

solution_56 :: Tree a -> Bool
solution_56 Empty          = True
solution_56 (Branch _ l r) = equal_tree l r


----------------------------------------------------------------------------
-- 1-22, 26-34, 55-56 == 1.9


-- (**) Binary search trees (dictionaries)
solution_57 :: Ord a => [a] -> Tree a
solution_57 = foldl insert_bst Empty
  where insert_bst Empty x = Branch x Empty Empty
        insert_bst (Branch v l r) x 
          | v == x = Branch v l r
          | v >  x = Branch v (insert_bst l x) r
          | v <  x = Branch v l (insert_bst r x) 


-- (**) Generate-and-test paradigm
-- Apply the generate-and-test paradigm to construct all symmetric,
-- completely balanced binary trees with a given number of nodes.
solution_58 :: Int -> [Tree Char]
solution_58 n = [t | t <- solution_55 n, solution_56 t]     -- this is slow

solution_58' :: Int -> [Tree Char]    -- a constructive building method which is much faster
solution_58' n
  | n `mod` 2 == 0 = []
  | otherwise = [Branch 'x' t (reverse_tree t) | t <- solution_55 (n `div` 2)]
      where reverse_tree Empty = Empty
            reverse_tree (Branch 'x' l r) = Branch 'x' (reverse_tree r) (reverse_tree l)


-- (**) Construct height-balanced binary trees
-- Construct a list of all height-balanced binary trees with the given element and the given maximum height.
-- no more than the maximum height
solution_59 :: Int -> [Tree Char]
solution_59 n = helper_59 n
  where helper_59 0 = [Empty]
        helper_59 1 = [Branch 'x' Empty Empty]
        helper_59 n = [Branch 'x' l r | (i, j) <- [(n-1, n-1), (n-1, n-2), (n-2, n-1)],
                                        l <- helper_59 i,
                                        r <- helper_59 j]

										
----------------------------------------------------------------------------
-- 1-22, 26-34, 55-59 == 1.10


-- minNodes :: Int -> Int
-- maxHeight :: 


data MTree a = Node a [MTree a]
  deriving (Eq, Show)

  
tree1 = Node 'a' []
tree2 = Node 'a' [Node 'b' []]
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ] 
  

-- (*) Count the nodes of a multiway tree.  
solution_70c :: MTree a -> Int
solution_70c (Node _ subs) = 1 + (sum $ map solution_70c subs)
  

-- (**) English number words
-- On financial documents, like cheques, numbers must sometimes be written in full words.
-- Example: 175 must be written as one-seven-five.
-- Write a predicate full-words/1 to print (non-negative) integer numbers in full words.
solution_95 :: Integer -> String
solution_95 = intercalate "-" . map num2eng . show

num2eng :: Char -> String
num2eng '1' = "one"
num2eng '2' = "two"
num2eng '3' = "three"
num2eng '4' = "four"
num2eng '5' = "five"
num2eng '6' = "six"
num2eng '7' = "seven"
num2eng '8' = "eight"
num2eng '9' = "nine"
num2eng '0' = "zero"


-- (**) Syntax checker
-- In a certain programming language (Ada) identifiers are defined by the syntax diagram below.
-- Transform the syntax diagram into a system of syntax diagrams which do not contain loops;
-- i.e. which are purely recursive. Using these modified diagrams, write a predicate identifier/1
-- that can check whether or not a given string is a legal identifier.
solution_96 :: String -> Bool
solution_96 [] = False
solution_96 (x:xs) = isLetter x && id_check xs               -- the first element of the name has different rule
  where id_check []  = True                                  -- so splite the rule with pattern matching
        id_check [x] = isAlphaNum x
        id_check (x:y:zs)
          | x == '-'     = (y /= '-') && id_check (y:zs)
          | isAlphaNum x = id_check (y:zs)
          | otherwise    = False
		  

----------------------------------------------------------------------------
-- 1-22, 26-34, 55-59, 70c, 95, 96 == 1.11
