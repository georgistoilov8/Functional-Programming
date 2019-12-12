-- Дали едно число е просто
isPrime :: Int -> Bool
isPrime n
  | n == 0 || n == 1 = False
  | n > 1            = isDivededBy [2..midN] n
  | otherwise        = error "Negative number"
 where midN = div n 2

isDivededBy :: [Int] -> Int -> Bool
isDivededBy [] _   = True
isDivededBy (x:xs) n = if modulo == 0 then False else isDivededBy xs n
  where modulo = mod n x

-- Най-голям общ делител
myGCD :: Integral a => a -> a -> a
myGCD 0 y = y
myGCD x 0 = x
myGCD x y
  | modulo == 0 = minNumber
  | modulo > 0  = myGCD minNumber modulo
  | otherwise   = error "Error"
  where maxNumber = max (abs x) (abs y)
        minNumber = min (abs x) (abs y)
        modulo = mod maxNumber minNumber

-- [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]  ->  [9,3,3]

-- Дали две числа са взаимно прости
coprime :: Integral a => a -> a -> Bool
coprime x y = if (gcd x y) == 1 then True else False
-- coprime 35 64 -> True