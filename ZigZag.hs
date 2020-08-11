module ZigZag where
  
{-

Problem 6
https://leetcode.com/problems/zigzag-conversion/

The string "PAYPALISHIRING" is written in a zigzag pattern on a 
given number of rows like this: (you may want to display this 
pattern in a fixed font for better legibility)

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

-------
{-
Solution #1.

The solution consists of two distinct operations.
First mark all characters from the string with their row numbers,
then pick all rows starting from the first.
Examples:

Input:  zigZag "PAYPALISHIRING" 3
Output: "PAHNAPLSIIGYIR"

Input:  zigZag "PAYPALISHIRING" 2
Output: "PYAIHRNAPLSIIG"
-}

zigZag :: String -> Int -> String
zigZag [] _ = []
zigZag xs 1 = xs
zigZag xs n =
  let
    ys = markRows xs n  -- Form pairs of chars with their row numbers
  in
    pickAllRows ys n  -- Pick chars from each row starting from the first

------
{-
Part 1:

Given a string and the number of rows, this function pairs each
character in the string with its row number
-}
markRows :: String -> Int -> [(Char, Int)]
markRows [] _ = []
markRows xs n = zip xs (concat $ repeat $ getWave n)

-------
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
{- 
Part 2. Picks all rows starting from the i-th one

Input example: [('A','1'),('B','2'),('C','3'),('D','1'),('E', 2)]
Output: "ADBEC" because A and D have level 1 whereas B and E
have level 2 and C has level 3
-}

pickAllRows :: [(Char, Int)] -> Int -> String
pickAllRows xs numRows = [ a | row <- [1..numRows], (a, r) <- xs, r == row]

-- ===========================
{-
Solution #2.

Part 1 is the same as before but Part 2 -- picking all rows
starting from the first row -- is done without using list
comprehensions. I implemented this before doing list comprehensions
as I was not too familiar with list comprehensions initially.
-}

pickAllRows' :: [(Char, Int)] -> Int -> Int -> String
pickAllRows' [] _ _ = ""
pickAllRows' xs n i
  | i == (n + 1)  = ""
  | otherwise     = (pickRow xs i) ++ pickAllRows' xs n (i + 1)

------
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
