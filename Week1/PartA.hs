import System.Random(randomRs,mkStdGen)
import Data.List

-- The Swedish Cake problem
-- Difficulty 🌶🌶

-- A cake is left in the common room for everyone to enjoy.
-- People never take more than one piece, but obej the
-- following protocol:

-- If there is more than 150g then they take a 100g slice.
-- If there is less than 30g then they take it all.
-- If there is between 30 and 150g then they take half of the remaining cake.


cake :: Int -> Int
cake a | a < 30    = 1
       | a > 150   = 1 + cake (a - 100)
       | otherwise = 1 + cake (a `div` 2)
 
-- Write a recursive function to compute the number
-- of people who will get a taste of a cake that
-- weighs x grams.
-- Try to write the type of the function before you start. 

-------------------------------------------------------
-- This questions is about the weekly sales from a shop,
-- where week numbers start at zero. 

-- sales i is the sales for week i.
-- Assume the week number (Int) is positive.
-- Sales is always non-negative.


sales :: Int -> Integer
sales i = randomRs (0,1000) (mkStdGen i) !! i
           -- sales in the range 0 - 1000
           -- The definition is not important!

-- Difficulty🌶️  Give recursive definitions of functions
-- which compute:

salesArray :: Int -> [Integer]
salesArray a = sales <$> [1..a]

totalSalesForNWeeks :: Int -> Integer 
totalSalesForNWeeks 0 = 0
totalSalesForNWeeks a = sales a + totalSalesForNWeeks (a - 1)

highestSale :: Int -> Integer 
highestSale 0 = 0
highestSale a = max (sales a) (highestSale (a - 1))

lowWeeks :: Int -> Integer 
lowWeeks 0 = 0
lowWeeks a | sales a < 100 = 1 + lowWeeks (a - 1)
           | otherwise     = lowWeeks (a - 1)

totalSalesForNWeeksLC :: Int -> Integer 
totalSalesForNWeeksLC a = sum $ salesArray a

highestSaleLC :: Int -> Integer 
highestSaleLC a = maximum $ salesArray a 

lowWeeksLC :: Int -> Integer 
lowWeeksLC a = toInteger $ length [n | n <- salesArray a, n < 100]

-- (1) total sales for the first n weeks?
-- (2) highest sale in the first n weeks?
-- (3) number of weeks with sales less than 100
--     in the first n weeks. 
-- (4) Define each of these using list comprehensions
--     instead of recursion

avgSales :: Int -> Integer 
avgSales 0 = 0
avgSales a = totalSalesForNWeeksLC a `div` toInteger a

-- Using recursion, indirectly or in a helper function:
-- Difficulty 🌶🌶️
-- (4) Average sales up to and including week n?

topWeeks :: Int -> Integer 
topWeeks a = [(w, sales w) | w <- [1..a]]


-- Difficulty 🌶🌶🌶
-- (5) Give the week numbers of the weeks with the best
-- sales in the first n weeks.
-- Since there may be more than one week with the best sales,
-- your answer should be a list of weeks.

-- (6) Write a quickcheck property for (5)
-- which checks that you included all the right weeks
-- in your answer, and none of the wrong ones!









