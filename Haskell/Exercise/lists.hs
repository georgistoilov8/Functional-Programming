-- Обикновен факториел
--fact 0 = 1
--fact x = x * fact(x-1)

-- Факториел с пазачи:
fact n
  | n == 0 = 1
  | n > 0  = n * fact(n - 1)
  | n < 0  = error "Factoriel of negative number"

fact2 n
  | n == 0 = 1
  | n > 0  = next
  | n < 0  = error "Factoriel of negative number"
 where next = n * fact2(n-1)

fib 0 = 1
fib 1 = 1
fib x = fib(x-1) + fib(x-2)


-- Последния елемент в лист:
myLast l = head(reverse l)
-- myLast = head . reverse

myLast' :: [a] -> a
myLast' []     = error "No end for empty lists!"
myLast' [x]    = x
myLast' (_:xs) = myLast' xs

-- myLast [1,2,3,4]      -> 4
-- myLast ['x','y','z']  -> 'z'

-- Предпоследен елемент в лист:
myButLast [x,_]  = x
myButLast (_:xs) = myButLast xs

-- myButLast [1,2,3,4]  -> 3
-- myButLast ['a'..'z'] ->'y'

-- Намиране на К-тия елемент от лист
elementAt [] k     = error "No such element at given position"
elementAt (x:_) 1  = x 
elementAt (_:xs) k = elementAt xs (k-1) 

-- elementAt [1,2,3] 2   -> 2
-- elementAt "haskell" 5 ->'e'

-- Намиране дължината на списък
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- myLength [123, 456, 789]  -> 3
-- myLength "Hello, world!"  -> 13

-- Обръщане на списък
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse(xs) ++ (x:[])

-- myReverse "A man, a plan, a canal, panama!"  -> "!amanap ,lanac a ,nalp a ,nam A"
-- myReverse [1,2,3,4]                          -> [4,3,2,1]

-- Дали даден списък е палиндром
-- Ползваме ограничението типът "а" да е Eq, което означава, че "a", може да е само тип, който при който може да се сравняват елементите
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []     = True
isPalindrome [_]    = True
isPalindrome (x:xs) = if x == myLast(xs) then isPalindrome(init(xs)) else False

-- isPalindrome [1,2,3]              -> False
-- isPalindrome "madamimadam"        -> True
-- isPalindrome [1,2,4,8,16,8,4,2,1] -> True

-- Премахване на последователни повторения на елементи от списък
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x == head(xs) then compress(xs) else x:compress(xs)

-- compress "aaaabccaadeeee" -> "abcade"

-- Дуплициране на елементите на списък
dupli :: [a] -> [a]
dupli []     = []
dupli [x]    = x:x:[]
dupli (x:xs) = x:x:dupli(xs)

-- dupli [1, 2, 3] -> [1,1,2,2,3,3]


--replicateX x k -- връща списък от К на брой Х-а.
replicateX :: a -> Integer -> [a]
replicateX _ 0 = []
replicateX x k = x:replicateX x (k-1)
-- Повтаряне на елементите К на брой пъти
repli :: [a] -> Integer -> [a]
repli [] _     = []
repli [x] k    = replicateX x k
repli (x:xs) k = replicateX x k ++ repli xs k

-- repli "abc" 3  -> "aaabbbccc"

-- Премахване на всеки N-ти елемент от списък
dropEvery :: [a] -> Int -> [a]
dropEvery [] _     = []
dropEvery [x] k    = if k == 1 then [] else [x]
dropEvery (xs) k = firstK ++ dropEvery newList k
  where newList = drop k xs
        firstK  = take (k-1) xs
--let newList          = drop k xs
--    firstK           = take (k-1) xs
--in  dropEvery xs k = firstK ++ dropEvery newList k

--  dropEvery "abcdefghik" 3 -> "abdeghk"

-- Разделяне на списък на два списъка, като знаем големината на първия
split xs n = [take n xs, drop n xs]

-- Премахване на елемент от дадена позиция в списък
removeAt :: Int -> [a] -> [[a]]
removeAt k xs = [[elementAt xs k], (take (k-1) xs) ++ (drop k xs)]
-- removeAt 2 "abcd" -> ('b',"acd")

-- Добавяне на елемент на дадена позиция в списък
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs k = take k xs ++ [x] ++ drop k xs
-- insertAt 'X' "abcd" 2 -> "aXbcd"

-- Създаване на списък, който съдържа всички числа в интервал
range :: Int -> Int -> [Int]
range x y
  | x == y    = [x]
  | x < y     = x:(range (x+1) y)
  | otherwise = error "Not interval"
-- range 4 9  -> [4,5,6,7,8,9]