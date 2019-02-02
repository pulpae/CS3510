-- (1)
double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns

-- (2)
listDivInt = a `div` length xs
       where
          a = 10
          xs = [1,2,3,4,5]

-- definitions must have the same spacing. Function wrapped in wrong symbols.
-- function name must begin by a lowercase letter.

-- (3)
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

-- (b)
qsortRev [] = []
qsortRev (x:xs) =
  qsortRev larger ++ [x] ++ qsortRev smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]
