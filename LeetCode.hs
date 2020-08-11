{- 
  Updated: August 11, 2020
  Ninad Jog
-}

module LeetCode where
import Data.Char
  
-------------------------------------------
-- LEET CODE

-- Problem 1
{-
https://leetcode.com/problems/two-sum/

Given an array of integers, return indices of the two numbers such that they add up to a specific target. Assume that each input would 
have exactly one solution, and you may not use the same 
element twice.

Example: Given nums = [2, 7, 11, 15], target = 9,
return [0, 1] because nums[0] + nums[1] = 2 + 7 = 9

There are 2 solutions: one obtained from zipping up the numbers
with their indices and the other without. A solution using list
comprehension might be a third one, but I haven't come up with it 
yet.
-}

---------
{-
Solution #1

Whenever we have to work with the positions of elements in 
a Haskell list (a.k.a. array indices), it's best to pair each
element with its index. This can be done by zipping the list
with [0..]

That's what the twoSumZip function does before passing on the
paired list to twoSumZip'
-}

------------
{-
Pair the input list with integers starting from 0.
Example:

Input:  [5, 4, -2]
Output: [(5, 0), (4, 1), (-2, 2)]

-}
twoSumZip :: [Int] -> Int -> [Int]
twoSumZip xs num = twoSumZip' ys num
  where 
    ys = zip xs [0..]

------------
{-
Takes the first number in the list and tries to find
a second number from the rest of the list that adds up
to the target. Calls the findNumIdx function to find the
index of such a number. findNumIdx returns -1 if such
a number is not found.

If such a number is not found, recursively move on to 
the next number in the list and repeat the search.
-}

twoSumZip' :: [(Int, Int)] -> Int -> [Int]
twoSumZip' [] _ = []
twoSumZip' ((x, idx1):xs) target  
  | idx2 /= -1  = [idx1, idx2]        -- base case
  | otherwise   = twoSumZip' xs target -- recursive case
  where
    idx2 = findNumIdx xs (target - x)
    
------------
{-
Given a number and a list of tuples containing numbers 
paired up with their indices, this function returns the index
of the number if the number is found, otherwise returns -1.

Example:
Inputs: [(5, 0), (4, 1), (-2, 2)] (-2)
Output: 2
-}

findNumIdx :: [(Int, Int)] -> Int -> Int
findNumIdx [] _ = -1
findNumIdx ((x, idx):xs) num
  | x == num  = idx
  | otherwise = findNumIdx xs num
  

-----------------------------
{-
Solution #2.

This was my first attempt at a solution before I thought of
solution #1. It should be just as performant as #1, but is
probably not as elegant.

twoSum is the main function. It calls a helper function, twoSum',
starting at index 0. twoSum' in turn calls a function named
findNumIndex.

Example:
twoSum [2, -7, 11, 15] 8 returns [1, 3] because -7 + 15 = 8

If the no two elements from the input list add up to the target
number, this function returns an empty list. For example,

twoSum [2, -7, 11, 15] 20
returns [] because no two numbers from the list add up to 20.
-}

twoSum :: [Int] -> Int -> [Int]
twoSum xs target = twoSum' xs target 0

---------
{-
This function does the actual work. It begins by taking the 
first number from the list and tries to find a number 
that equals targetSum MINUS firstNumber in the remainder of
the list (so that firstNum + that number = targetSum)

If findNum returns a non-negative number, it means such a
number has been found 
If such a number is not found (i.e. if findNum returns -1), 
it moves on to the next number in the list.
-}

twoSum' :: [Int] -> Int -> Int -> [Int]
twoSum' [] _ _ = []
twoSum' (x:xs) target idx1 
  | idx2 /= -1  = [idx1, idx1 + idx2 + 1]      -- base case
  | otherwise   = twoSum' xs target (idx1 + 1) -- recursive case
  where
    idx2 = findNumIndex xs (target - x) 0

----------
{-
Find the index (position) of the given number in the given list
Return -1 if the number is not found. Instead of returning
-1 if the number is not found, we could have changed the return
type of this function to Maybe Int and returned None.

Examples:
findNum [0, 1, 2, 3, 4] 3 0 
returns 3 because the index of 3 in the list is 3

findNum [5, 4, -3] 2 0
returns -1 becuse 2 does not appear in the list
-}

findNumIndex :: [Int] -> Int -> Int -> Int
findNumIndex [] _ _ = -1     -- If number is not found
findNumIndex (x:xs) num pos
  | x == num = pos
  | otherwise = findNumIndex xs num (pos+1)
  
-----------------------------
{-
Solution #3
See whether we can use a list comprehension
-}

-- This method is not currently used. Just exploring.
pairs :: Int -> [(Int, Int)]
pairs n = [(a, b) | a <- [0..n-1], b <- [a+1..n-1]]

sums :: [Int] -> Int -> [(Int, Int)]
sums ys@(x:xs) num = [(a, b) | a <- ys, b <- xs, a+b == num]


-- ==========================================================
{-
Problem 2.

https://leetcode.com/problems/add-two-numbers/

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

https://leetcode.com/problems/zigzag-conversion/

The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows like this: (you may want to display this pattern in a fixed font for better legibility)

P     A       H       N
 A  P   L   S   I   I   G
  Y       I       R
And then read line by line: "PAHNAPLSIIGYIR"

----
Example 2:

Input: s = "PAYPALISHIRING", numRows = 4
Output: "PINALSIGYAHRPI"
Explanation:

P           I           N
  A       L   S       I   G
    Y   A       H   R
      P           I

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
