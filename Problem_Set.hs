-- 99 questions
-- 2015.1.4 .. Trevillie

module Problem_Set where

import Data.List

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
----------------------------------------------------------------------------
---------                                                        -----------
---------    This Marks The End Of The First Day  -  2015.1.5    -----------
---------                                                        -----------
----------------------------------------------------------------------------
----------------------------------------------------------------------------




