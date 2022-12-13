-- Ole Bergens, 221200097
-- Blazej Schott, 221200610
-- Antonin Gräser, 221201792

-- add (3,2): hat den Typ Int, da add eine Funktion ist, die zwei Int-Werte als Argument nimmt und einen Int-Wert zurückgibt. 
-- addC 3 2 : hat den Typ Int, da addC eine Funktion ist, die einen Int-Wert als erstes Argument und zweites Argument nimmt und einen Int-Wert zurückgibt. 
-- addC (succ 2) 2: hat den Typ Int, da addC eine Funktion ist, die einen Int-Wert als erstes und zweites Argument nimmt und einen Int-Wert zurückgibt. Die Anwendung von der Funktion succ 2 (Ergebnis ist 3) und 2 liefert dann ja einen Int-Wert.

-- Definition der nor-Funktion

nor :: Bool -> Bool -> Bool
nor x y = not (x||y)

nor2 :: Bool -> Bool -> Bool
nor2 False False = True
nor2 _ _ = False 

-- Schrittweise Auswertung der nor-Funktion

-- nor False False = not (False || False)
-- = not False
-- = True

-- nor2 False False = True
-- = True

-- nor False True = not (False || True)
-- = not True
-- = False

-- nor2 False True = nor2 _ _ = False
-- = False

-- Schrittweise Auswertung der fib Funktion
-- fib 4 = fib (4-1) + fib (4-2)
-- fib (3) + fib (2) = fib (3-1) + fib (3-2) + fib (2-1) + fib (2-2)
-- fib (3-1) + fib (3-2) + fib (2-1) + fib (2-2) = fib (2) + fib (1) + fib (1) + fib (0)
-- fib (2) + fib (1) + fib (1) + fib (0) = fib (2-1) + fib (2-2) + 1 + 1 + 0
-- fib (1) + fib (0) + 1 + 1 + 0 = 1 + 0 + 1 + 1 + 0
-- = 3

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]
perfekt :: Int -> [Int]
perfekt n = [x | x <- [1..n*2], sum (divisors x) == 2*x]
