module Lists where


-- | Problem 1
-- Find the last element of a list
-- 
-- >>> myLast [1,2,3,4]
-- 4
-- >>> myLast ['x','y','z']
-- 'z'
myLast :: [a] -> a
myLast = head . reverse


-- | Problem 2
-- Find the last but one element of a list. 
--
-- >>> myButLast [1,2,3,4]
-- 3
-- >>> myButLast ['a'..'z']
-- 'y'
myButLast :: [a] -> a
myButLast = head . tail . reverse


-- | Problem 3
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
-- Find the number of elements of a list. 
--
-- >>> myLength [123, 456, 789]
-- 3
-- >>> myLength "Hello, world!"
-- 13
myLength :: [a] -> Int
myLength = foldl (\n _ -> n + 1) 0


-- | Problem 5
-- Reverse a list.
--
-- >>> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- >>> myReverse [1,2,3,4]
-- [4,3,2,1]
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


-- | Problem 6
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
