{-
    Example of evaluation in Haskell:

    sumFirst (x:xs) (y:ys) = x + y
       sumFirst [1..10] [5..50]
    => (\(x:xs) -> \(y:ys) -> x + y) [1..10] [5..50]
    => (\(x:xs) -> \(y:ys) -> x + y) (1:[2..10]) [5..50]
    => let x=1; xs = [2..10] in (\(y:ys) -> x + y) [5..50]
    => let x=1; xs = [2..10] in (\(y:ys) -> x + y) (5:[6..50])
    => let x=1; xs = [2..10]; y = 5; ys = [6..50] in x + y
    => 1 + 5 => 6
-}

{-
    Streams

    In Haskell we can assume that the arguments are promises which are forced when necessary
    Lists in Haskell are actually streams
    x:xs = (:) where
        x is a promise for head
        xs is a promise for tail

    Endless streams
        ones = 1 : ones
        length ones -> ...
        take 5 ones -> [1,1,1,1,1]

    Generation of endless stream:
    [a..] -> [a, a+1, a+2, ...]
    Examples:
        nats = [0..]
        take 5 [0..] -> [0,1,2,3,4]
        take 26 ['a'..] -> English alphabet
    Syntactic sugar for enumFrom from

    [a, a+dx..] -> [a, a+dx, a+dx+dx, ...]
    Examples:
        evens = [0, 2..]
        take 5 evens -> [0,2,4,6,8]
        take 7 ['a', 'e'..] -> "aeimquy"
    Syntactic sugar for enumFromThen from then

    repeat :: a -> [a] => [x,x,...]
    repeat x = x : repeat x

    replicate n x = take n (repeat x)

    cycle :: [a] -> [a]
    cycle [1,2,3] -> [1,2,3,1,2,3,...]

    iterate :: (a->a) -> a -> [a]
    iterate f z -> [z, f(z), f(f(z)),...]
    iterate f z = z : iterate f (f z)
-}

oddSquares = [ x^2 | x <- [1,3..] ]

--twins = [ (x, x+2) | x <- [1..], isPrime x, isPrime (x + 2) ]

pairs = [ (x,y) | x <- [0..], y <- [0..x - 1]]

pythagoreanTriples = [ (a,b,c) | c <- [1..], b <- [1..c-1], a <- [1..b-1], a^2+b^2 == c^2]

{-
    High order functions

    Examples: 
    powers2 = 1 : map (*2) powers2
    fibs = 0:1:zipWith (+) fibs (tail fibs)

    In Haskell we have $ syntax. For example:
    (...((f x1) x2)...xn) = f x1 x2 ... xn
    but
    f1(f2...(fn x)...) = f1 $ f2 $ ... $ fn $ x

    head (tail (take 5 (drop 7 l))) == head $ tail $ take 5 $ drop 7 $ l
    sum (map (^2) (filter odd [1..10])) == sum $ map (^2) $ filter odd $ [1..10]

    (f . g) x = f (g x) - composition
    Examples:
        sublist n m l = take m (drop n l) == sublist n m = take m . drop n
        sumOddSquares l = sum (map (^2) (filter odd l)) == sumOddSquares = sum . map (^2) . filter odd
-}