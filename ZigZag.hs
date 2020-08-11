module ZigZag where
  
import Control.Monad

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

Solutions:
The 3 solutions use list comprehension, list monad, and
structural recursion.
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
    pickRows ys n  -- Pick chars from each row starting from the first

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

pickRows :: [(Char, Int)] -> Int -> String
pickRows xs numRows = [ a | row <- [1..numRows], (a, r) <- xs, r == row]

-- ===========================
{-
Solution #2

Part 1 is the same as before but the pickAllRows function of Part 2
is implemented with a List Monad rather than a list comprehension
as follows.
-}

pickRowsM :: [(Char, Int)] -> Int -> String
pickRowsM xs numRows = do
  row     <- [1..numRows]
  (a, r)  <- xs
  guard (r == row)
  return a


-- ===========================
{-
Solution #3

Part 1 is the same as before but Part 2 -- picking all rows
starting from the first row -- is done without using list
comprehensions. I implemented this before doing list comprehensions
as I was not too familiar with list comprehensions initially.

The following two functions use structural recursion.
-}

pickRows' :: [(Char, Int)] -> Int -> Int -> String
pickRows' [] _ _ = ""
pickRows' xs n i
  | i == (n + 1)  = ""
  | otherwise     = (pickRow xs i) ++ pickRows' xs n (i + 1)

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
  
-- ===========================
{-
Solution #4 (Not implemented)

Another solution would be to use a formula to compute the row
number of each character in the input string based on its
position in the string.

The second part (picking rows starting from the first row)
will be the same as above: either use list comprehensions or
list monads or structural recursion.
-}
