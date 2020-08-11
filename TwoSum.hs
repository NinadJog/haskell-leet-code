module TwoSum where
  
import Data.Char

{-

Problem 1
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
  

-- ===========================
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
  
-- ===========================
{-
Solution #3
See whether we can use a list comprehension
-}

-- This method is not currently used. Just exploring.
pairs :: Int -> [(Int, Int)]
pairs n = [(a, b) | a <- [0..n-1], b <- [a+1..n-1]]

sums :: [Int] -> Int -> [(Int, Int)]
sums ys@(x:xs) num = [(a, b) | a <- ys, b <- xs, a+b == num]

