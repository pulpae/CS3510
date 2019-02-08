-- LAB O1

-- (1) Defining simple functions:
double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns

-- (2) Dividing an integer by the length of a list:
listDivInt = a `div` length xs
       where
          a = 10
          xs = [1,2,3,4,5]

-- consecutive definitions must have the same margin.
-- function wrapped in wrong symbols.
-- function name must begin by a lowercase letter.

-- (3) Sorting:
qsort [] = []
qsort (x:xs) =
  qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- (a) Sorting lists with duplicate values would only include each value once.
-- If x would be equal to the previous value, it would not be taken into account
-- because of the comparisons a<x and b>x. A larger or equal to operator must be
-- included instead (a<=x).

-- (b) Descending/reverse sorting:
qsortRev [] = []
qsortRev (x:xs) =
  qsortRev larger ++ [x] ++ qsortRev smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- (4) Defining 'last' using other functions:

selLast xs = xs !! (length xs-1)

revHead xs = head (reverse xs)

-- LAB 02
-- (1) (a) Types
-- ['a', 'b'] || Char [Char, Char]
-- ('a', 'b') || Char (Char, Char)
-- [(False, '0'), (True, '1')] || Tuple [(Bool, Char), (Bool, Char)]
-- ([False, True], ['0', '1']) || Tuple (Bool, Char)
-- [tail, init, reverse] || function

second xs = head (tail xs)
-- [a] -> a || type variable (any type)

swap(x,y) = (y,x)
-- (b,a) -> (a,b) || tuple

pair x y = (x,y)
-- a->b->(a,b) || tuple

doublee x = x*2
-- Num a => a -> a || Num (as per the class constraint)

palindrome xs = reverse xs == xs
-- Eq a => [a] -> Bool || Boolean

twice f x = f (f x)
-- (t->t)->t->t || type variable (function f might have a specific type).

-- (2) [[False, True], ['0','1']] || A list can only hold values of the same type.
-- This would be [Bool, Char]. A tuple should be used instead (Bool, Char).

--(3) Defining functions

doubleAll xs = map (*2) xs

isEven n = n `mod` 2 == 0



halve xs | isEven(length xs) = (take n xs, drop n xs)
         | otherwise = ([],[])  
         where n = (length xs) `div` 2
