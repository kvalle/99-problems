module Lists where

-- | Find the last element of a list
-- 
-- >>> myLast [1,2,3,4]
-- 4
-- >>> myLast ['x','y','z']
-- 'z'
myLast :: [a] -> a
myLast = head . reverse
