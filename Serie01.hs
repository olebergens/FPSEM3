import Data.List (sort)

-- Ole Bergens, 221200097
-- Blazej Schott, 221200610
-- Antonin Gräser, 221201792
-- Ivo Hansen, 221200192

-- Aufgabe 1
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = a == b && b == c && c == d
-- Aufgabe 2
orderTriple :: Ord a => (a, a, a) -> (a, a, a)
orderTriple (x, y, z) = 
 let [a, b , c] = sort [x, y, z] in (a, b, c)
-- Aufgabe 3
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)
-- Aufgabe 4
isElem :: Integer -> [Integer] -> Bool
isElem _ [] = False
isElem x (y : ys) = (x == y) || isElem x ys
-- Aufgabe 5
luhnDouble :: Integer -> Integer
luhnDouble x = if (2 * x) > 9 then (2 * x) - 9 else 2 * x
luhn :: Integer -> Integer -> Integer -> Integer -> Bool
luhn a b c d = 0 == sum[luhnDouble a, b, luhnDouble c, d] `mod` 10
