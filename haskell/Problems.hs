module Problems where

import System.Random (mkStdGen, randomRs, RandomGen)
import Debug.Trace

-- | Problem 1
--
-- Find the last element of a list
-- 
-- >>> myLast [1,2,3,4]
-- 4
-- >>> myLast ['x','y','z']
-- 'z'
myLast :: [a] -> a
myLast = head . reverse


-- | Problem 2
--
-- Find the last but one element of a list. 
--
-- >>> myButLast [1,2,3,4]
-- 3
-- >>> myButLast ['a'..'z']
-- 'y'
myButLast :: [a] -> a
myButLast = head . tail . reverse


-- | Problem 3
--
-- Find the K'th element of a list. The first element in the list is number 1. 
--
-- >>> elementAt [1,2,3] 2
-- 2
-- >>> elementAt "haskell" 5
-- 'e'
elementAt :: [a] -> Int -> a
elementAt xs 1 = head xs
elementAt xs n = elementAt (tail xs) (n - 1)


-- | Problem 4
--
-- Find the number of elements of a list. 
--
-- >>> myLength [123, 456, 789]
-- 3
-- >>> myLength "Hello, world!"
-- 13
myLength :: [a] -> Int
myLength = foldl (\n _ -> n + 1) 0


-- | Problem 5
--
-- Reverse a list.
--
-- >>> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- >>> myReverse [1,2,3,4]
-- [4,3,2,1]
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


-- | Problem 6
--
-- Find out whether a list is a palindrome. 
-- A palindrome can be read forward or backward; e.g. (x a m a x).
--
-- >>> isPalindrome [1,2,3]
-- False
-- >>> isPalindrome "madamimadam"
-- True
-- >>> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = reverse xs == xs


-- | Problem 7
--
-- Flatten a nested list structure: Transform a list, possibly holding lists as elements into 
-- a `flat' list by replacing each list with its elements (recursively). 
--
-- >>> flatten (Elem 5)
-- [5]
-- >>> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- >>> flatten (List [])
-- []
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concat $ map flatten xs

data NestedList a = Elem a | List [NestedList a]
    deriving (Show)


-- | Problem 8
--
-- Eliminate consecutive duplicates of list elements.
-- 
-- If a list contains repeated elements they should be replaced with a single copy of the element. 
-- The order of the elements should not be changed. 
-- >>> compress "aaaabccaadeeee"
-- "abcade"
compress :: Eq a => [a] -> [a]
compress = (map head) . pack


-- | Problem 9
--
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated 
-- elements they should be placed in separate sublists.
-- >>> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = (takeWhile eq xs):(pack $ dropWhile eq xs)
    where eq x = x == head xs


-- | Problem 10
--
-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called 
-- run-length encoding data compression method. Consecutive duplicates of elements are encoded 
-- as lists (N E) where N is the number of duplicates of the element E.
--
-- >>> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\xs -> (myLength xs, head xs)) . pack


-- | Problem 11
--
-- Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply 
-- copied into the result list. Only elements with duplicates are transferred as (N E) lists.
--
-- >>> encodeModified "fffffffuuuuuuuuuuuuu"
-- [Multiple 7 'f',Multiple 13 'u']
-- >>> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map foo . encode
    where foo (1, a) = Single a
          foo (n, a) = Multiple n a

data Encoded a = Single a | Multiple Int a 
    deriving (Show)


-- | Problem 12
--
-- Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11. 
-- Construct its uncompressed version.
--
-- >>> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified :: [Encoded a] -> [a]
decodeModified = concatMap decode
    where decode (Multiple n a) = (replicate n a)
          decode (Single a) = [a]


-- | Problem 13
--
-- Run-length encoding of a list (direct solution).
--
-- Implement the so-called run-length encoding data compression method directly. 
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count 
-- them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
--
-- >>> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect = map foo . count
    where foo (1, a) = Single a
          foo (n, a) = Multiple n a

count :: Eq a => [a] -> [(Int,a)]
count = foldr helper []
    where
        helper x [] = [(1,x)]
        helper x ((n,x'):rest)
            | x == x'   = ((n+1),x'):rest
            | otherwise = (1,x):(n,x'):rest


-- | Problem 14
-- 
-- Duplicate the elements of a list.
--
-- >>> dupli [1, 2, 3]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli [] = []
dupli (a:rest) = a:a:(dupli rest)


-- | Problem 15
--
-- Replicate the elements of a list a given number of times.
--
-- >>> repli "abc" 3
-- "aaabbbccc"
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs


-- | Problem 16
--
-- Drop every N'th element from a list.
--
-- >>> dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = (take (n-1) xs) ++ (dropEvery (drop n xs) n)


-- | Problem 17
--
-- Split a list into two parts; the length of the first part is given.
--
-- Do not use any predefined predicates.
--
-- >>> split "abcdefghik" 3
-- ("abc","defghik")
split :: [a] -> Int -> ([a], [a])
split xs     0 = ([], xs)
split (x:xs) n = let (h,t) = split xs (n-1)
                  in (x:h, t)


-- | Problem 18
--
-- Extract a slice from a list.
--
-- Given two indices, i and k, the slice is the list containing the elements 
-- between the i'th and k'th element of the original list (both limits included). 
-- Start counting the elements with 1.
-- 
-- >>> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
slice :: [a] -> Int -> Int -> [a]
slice xs a b = drop (a-1) $ take b xs


-- | Problem 19
--
-- Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
-- 
-- >>> rotate ['a','b','c','d','e','f','g','h'] 3
--"defghabc"
--
-- >>> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
rotate :: [a] -> Int -> [a]
rotate xs n | n >= 0    = (drop n xs) ++ (take n xs)
            | otherwise = let n' = (length xs) + n
                           in rotate xs n'


-- | Problem 20
-- 
-- Remove the K'th element from a list.
--
-- >>> removeAt 2 "abcd"
-- ('b',"acd")
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = let (as, b:bs) = splitAt (n-1) xs
                 in (b, as++bs)


-- | Problem 21
--
-- Insert an element at a given position into a list.
--
-- >>> insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n-1) xs) ++ x:(drop (n-1) xs)


-- | Problem 22
--
-- Create a list containing all integers within a given range.
-- 
-- >>> range 4 9
-- [4,5,6,7,8,9]
range :: Int -> Int -> [Int]
range a b = [a..b]


-- | Problem 23
--
-- Extract a given number of randomly selected elements from a list.
--
-- Note: Changed the task slightly to accept RandomGen as argument.
--
-- >>> import System.Random
-- >>> rndSelect (mkStdGen 1) "abcdefgh" 3
-- "bad"
rndSelect :: RandomGen g => g -> [a] -> Int -> [a]
rndSelect gen xs n = take n [ xs !! x | x <- indices]
    where indices = randomRs (0, (length xs) - 1) gen


-- | Problem 24
--
-- Lotto: Draw N different random numbers from the set 1..M.
-- 
-- >>> import System.Random
-- >>>lotto (mkStdGen 1) 6 49
-- [45,48,35,18,29,11]
lotto :: RandomGen g => g -> Int -> Int -> [Int]
lotto gen n m = take n $ unique $ randomRs (1, m) gen
    where unique []     = []
          unique (x:xs) = x : unique (filter (x /=) xs)


-- | Problem 25
-- 
-- Generate a random permutation of the elements of a list.
-- 
-- >>> import System.Random
-- >>> rndPerm (mkStdGen 1) "abcdef"
-- "febcda"
rndPerm :: RandomGen g => g -> [a] -> [a]
rndPerm gen source = [ source !! (x-1) | x <- indices]
    where indices = lotto gen n n
          n = length source


-- | Problem 26
--
-- Generate the combinations of K distinct objects chosen from the N elements 
-- of a list.
--
-- In how many ways can a committee of 3 be chosen from a group of 12 people? 
-- We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the 
-- well-known binomial coefficients). For pure mathematicians, this result may 
-- be great. But we want to really generate all the possibilities in a list.
--
-- >>> combinations 2 "abcdef"
-- ["ab","ac","ad","ae","af","bc","bd","be","bf","cd","ce","cf","de","df","ef"]
-- >>> length $ combinations 4 [1..50]
-- 230300
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [ (xs !! i):xs' | i <- [0..(length xs)-1],
                                      xs' <- combinations (n-1) (drop (i+1) xs)]


