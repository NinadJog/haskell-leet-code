module LeetCode where
import Data.Char
  
-------------------------------------------
-- LEET CODE

-- Problem 1
{-
Given an array of integers, return indices of the two numbers such that they add up to a specific target.

Assume that each input would have exactly one solution, and you may not use the same element twice.
-}

twoSum :: [Int] -> Int -> Int -> [Int]
twoSum [] _ _ = []
twoSum (x:xs) target idx1 
  | idx2 /= -1  = [idx1, idx1 + idx2 + 1]
  | otherwise   = twoSum xs target (idx1 + 1)
  where
    idx2 = findNum xs (target - x) 0

----------
-- Find the index (position) of the given number in the given list

findNum :: [Int] -> Int -> Int -> Int
findNum [] _ _ = -1     -- If partner is not found
findNum (x:xs) num pos
  | x == num = pos
  | otherwise = findNum xs num (pos+1)
  
---------------------------
{-
Problem 2.

You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order and each of their nodes contain a single digit. Add the two numbers and return it as a linked list.

You may assume the two numbers do not contain any leading zero, except the number 0 itself.

Example:
Input: (2 -> 4 -> 3) + (5 -> 6 -> 4)
Output: 7 -> 0 -> 8
Explanation: 342 + 465 = 807

Input:  addTwo [2, 4, 3] [5, 6, 4]
Output: [7, 0, 8]

-}

decodeNum :: [Int] -> Int
decodeNum [] = 0
decodeNum xs = decodeNum' xs 0
  where
    decodeNum' :: [Int] -> Int -> Int
    decodeNum' [] _ = 0
    decodeNum' (x:xs) pow = x * 10^pow + decodeNum' xs (pow + 1)
    
----

addTwo :: [Int] -> [Int] -> [Int]
addTwo xs ys = encodeNum $ show $ (decodeNum xs) + (decodeNum ys)

----
-- Given a string such as "807", convert it to a list of Int,
-- [7, 0, 8] in flipped order

encodeNum :: [Char] -> [Int]
encodeNum [] = []
encodeNum (x:xs) = (encodeNum xs) ++ [(ord x) - 48]

-- =======================================================
{-

Problem 6.

The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows like this: (you may want to display this pattern in a fixed font for better legibility)

P   A   H   N
A P L S I I G
Y   I   R
And then read line by line: "PAHNAPLSIIGYIR"

----
Example 2:

Input: s = "PAYPALISHIRING", numRows = 4
Output: "PINALSIGYAHRPI"
Explanation:

P     I    N
A   L S  I G
Y A   H R
P     I

-}

--------
{-
Returns a range in ascending order from 1 to n followed
by a descent down to 2
Given 4, it returns [1, 2, 3, 4, 3, 2]
Given 3, it returns [1, 2, 3, 2]
-}

getWave :: Int -> [Int]
getWave 1 = [1]
getWave n = [1..n] ++ [n-1, n-2..2]

------
markRows :: String -> Int -> [(Char, Int)]
markRows [] _ = []
markRows xs n = zip xs (concat $ repeat $ getWave n)

---
{- Picks all rows starting from the i-th one

Input example: [('A','1'),('B','2'),('C','3'),('D','1'),('E', 2)]
Output: "ADBEC" because A and D have level 1 whereas B and E
have level 2 and C has level 3
-}

pickAllRows :: [(Char, Int)] -> Int -> Int -> String
pickAllRows [] _ _ = ""
pickAllRows xs n i
  | i == (n + 1)  = ""
  | otherwise     = (pickRow xs i) ++ pickAllRows xs n (i + 1)


--------------------------
{- 
Extract characters that have the specified row. For example,
pickRow [('A', 1),('B', 2),('C', 3),('D', 1)] 1 = "AD"
because A and D have row 1.
-}

pickRow :: [(Char, Int)] -> Int -> String
pickRow [] _               = ""
pickRow ((ch, r):xs) row
  | r == row  = ch : pickRow xs row
  | otherwise = pickRow xs row

----
zigZag :: String -> Int -> String
zigZag [] _ = []
zigZag xs 1 = xs
zigZag xs n = pickAllRows (markRows xs n) n 1
