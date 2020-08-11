{- 
  Updated: August 9, 2020
  Ninad Jog
-}

module AddTwoNum where
  
import Data.Char

{-

Problem 2
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

----

addTwo :: [Int] -> [Int] -> [Int]
addTwo xs ys = encodeNum $ show $ (decodeNum xs) + (decodeNum ys)

----
{-
Given a string such as "807", convert it to a list of Int,
[7, 0, 8] in flipped order
-}

encodeNum :: [Char] -> [Int]
encodeNum [] = []
encodeNum (x:xs) = (encodeNum xs) ++ [(ord x) - 48]

----
{-
Converts a list of Int into an Integer in flipped order
Example: Converts [2, 4, 3] into the number 342.
-}

decodeNum :: [Int] -> Int
decodeNum [] = 0
decodeNum xs = decodeNum' xs 0
  where
    decodeNum' :: [Int] -> Int -> Int
    decodeNum' [] _ = 0
    decodeNum' (x:xs) pow = x * 10^pow + decodeNum' xs (pow + 1)
