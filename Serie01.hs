import Data.List (sort)
-- Aufgabe 1
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = a == b && b == c && c == d

-- Aufgabe 2
orderTriple :: Ord Integer => (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x, y, z) = 
	let [a, b , c] = sort [x, y, z]
	in (a, b, c)
	


-- Aufgabe 3
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)