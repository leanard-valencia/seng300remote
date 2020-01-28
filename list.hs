module HaskellLists where
import Test.QuickCheck  

sum_to :: Integer -> Integer -> Integer 
{- sum_to x y 
    | x < y = x + sum_to (x + 1) y 
    | otherwise = x 
--}

sum_to x y = sum [x..y]

my_list :: [Integer]
my_list = [1..20] 

custom_list :: (Integer, Integer, Bool) -> [Integer] 
custom_list (x, y, isodd) = [n | n <- [x..y], odd n == isodd] 

multiplication_line :: Integer -> [Integer] 
multiplication_line x = [x*n | n <- [1..10]]

multiplication_table :: [Integer] -> [[Integer]]
multiplication_table x = [multiplication_line n | n <- x]

double_head :: [Integer] -> [Integer] 
double_head x = [(head x)* 2] ++ tail x 

double_head2 :: [Integer] -> [Integer] 
double_head2 (x:xs) = [x*2] ++ xs 

square_odd :: [Integer] -> [Integer] 
square_odd x = [ n* n | n <- x, odd n]

square_odd_all :: [Integer] -> [Integer] 
square_odd_all (x:xs) 
--        | xs == [] && odd x 	    = [x*x]
--	| xs == [] && not (odd x)   = [x] 
	| odd x = [x*x] ++ square_odd_all xs 
	| otherwise = [x] ++ square_odd_all xs 
square_odd_all [] = [] 

prop_sq:: [Integer] -> Bool
prop_sq x = (length x) == (length (square_odd_all x))

my_square :: Integer -> Integer 
my_square x = x*x

my_cube :: Integer -> Integer 
my_cube x = x*x*x

prop_non_negative :: Integer -> Bool
prop_non_negative x = (my_cube x) >= 0 
