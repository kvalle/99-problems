module Problems where


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
