-- Tuples and lists

{-  Tuples
    Examples: (1, 2), (3.5, 'A', False), (("sqaure", (^2)), 1.0)
    We have n elements: (t1, t2, ..., tn) where the type of element xi is of type ti
    With tuples we can have more than one type together

    (,) :: a -> b -> (a,b) - construct a pair
    fst :: (a,b) -> a - first element of the pair
    snd :: (a,b) -> b - second element of the pair

    a = (1,"Georgi")
    fst a => 1
    snd a => "Georgi"
-}

{-  Types
    In real life if we have the tuple: (String, Int) then we can't tell what this tuple should contain
    For example it can contain name of a student and a grade or model of a car and it's horse powers.
    For convenience we can give names to each types.
    
    type <Constructor> = <Type>

    Example:
        type Student = (String, Int, Double)
        type Point = (Double, Double)
        type Triangle = (Point, Point, Point)
        type Vector = Point
-}

type Point = (Double, Double)
type Vector = Point

addVectors :: Vector -> Vector -> Vector
addVectors v1 v2 = (fst v1 + fst v2, snd v1 + snd v2)

{- 
    fst (1, 2, 3) -> Error
    fst and snd works only for pairs

    () - empty tuple
-}

{-  Model of a tuple
    addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    fst (x, _) = x
    snd (_, y) = y
    getFN :: Student -> Int
    getFN (_, fn, _) = fn

    We use models of the tuples to get part of the tuple that we actually care about
-}

betterStudent (name1, fn1, grade1) (name2, fn2, grade2)
  | grade1 > grade2 = (name1, fn1, grade1)
  | otherwise       = (name2, fn2, grade2)

{-  But what happens when we have 15 fields? 
    We can use named models for this job.
    <Name>@<Model>
-}

betterStudent2 s1@(_, _, grade1) s2@(_, _, grade2)
  | grade1 > grade2 = s1
  | otherwise       = s2


{- Lists -}
{- 
    [] is a list of type [a]
    If h is of type a and t is of type [a] then (h : t) is list of type [a] where:
        h - head of the list
        t - tail of the list
    Lists are a sequence with random length but with elements from one type

    (1:(2:(3:(4:[])))) = 1:2:3:4:[] != ((((1:2):3):4):[])
-}

{- Strings 
    type String = [Char]
    Examples:
        ['H', 'e', 'l', 'l', 'o'] == "Hello"
        'H':'e':"llo" == "Hello"
        ["12", ['3'], []] :: [String]
-}

{- Functions for lists

    head :: [a] -> a - head of the list
    head [[1,2], [3,4]] -> [1,2]
    head [] -> Error

    tail :: [a] -> a - tail of the list
    tail [[1,2], [3,4]] -> [[3,4]]    
    tail [] -> Error

    null [a] -> Bool - check if the list is empty 
    length [a] -> Int - returns the length of the list
-}

{- Models for lists 

    Examples:
        head (h:_) = h
        tail (_:t) = t
        null [] = True
        null _ = False
        length [] = 0
        length (_:t) = 1 + length t

-}

{- Case of models

    case <Expression> of <Model1> -> <Expression1>
                         <Model2> -> <Expression2>
                         ...
                         <ModelN> -> <Expression2>

-}

{- List Generators

    [a..b] -> [a, a+1, ..., b]
    Syntatic sugar for enumFromTo from to

    [a, a+dx..b] -> [a, a+dx, a+dx+dx, ..., b]
    Syntatic sugar for enumFromThenTo from then to

-}

{- Recursive functions
    
    (++) :: [a] -> [a] -> [a] - concat two lists
    [1..3] ++ [5..7] -> [1,2,3,4,5,6,7]

    reverse :: [a] -> [a] - reverse the list
    reverse [1..3] -> [3,2,1]

    (!!) :: [a] -> Int -> a - element at given index
    [1..5] !! 3 -> 4

    elem :: Eq a => a -> [a] -> Bool - check if the element contains in the list
    3 'elem' [1..5] -> True

-}

{- Typeclasses
    Eq is typeclass
    Eq is typeclass which has operations == and /=
        We can imagine that typeclasses are like interfaces
    Eq t - class constraint for type t
    Instantiation is every type for which the operations are given
    Instantiations of Eq:
        Bool, Char, Int, Integer, Float, Double

    Eq - Types with equality
    Ord - Types with order
        - operations ==, /=, >=, <=, <, >
        - special function compare
        - functions min and max
    Show - Types whose elements can be strings
        - function show :: a -> String
    Read - Types whose elements can be inserted from strings
        - function read :: String -> a
    Num - Numeric types
    Integral - Integer types
    Floating - Types with floating point
-}

{- List comprehension
    [<Expression> | <Generator> {, <Generator>} {, <Condition>}]
    <Generator> is of type: <Model> <- <Expression> where
        - <Expression> is list of type [a]
        - <Model> - is for elements of type a
    <Condition> - random expression of type Bool

    Examples: 
    [2 * x | x <- [1..5]] -> [2, 4, 6, 8, 10]
    [x^2 | x <- [1..10], odd x] -> [1,9,25,49,81]

    If we have more than one generator then we generate all combinations(Cartesian product)
    [x++(' ':y) | x<-["green", "blue"], y<-["sky", "grass"]] -> ["green sky", "green grass", "blue sky", "blue grass"]
-}

{- List cut
    init :: [a] -> [a] - returns the list without the last element
        - init [1..5] = [1,2,3,4]
    
    last :: [a] -> a - returns the last element of the list
        - last [1..5] = 5
    
    take :: Int -> [a] -> [a] - returns first N elements of a list
        - take 4 [1..10] = [1,2,3,4]

    drop :: Int -> [a] -> [a] - returns the list without the first N elements
        - drop 2 [1,3..10] -> [5,7,9]
-}

{- Functions
    maximum :: Ord a => [a] -> a
    minimum :: Ord a => [a] -> a
    sum :: Num a => [a] -> a - sum of numbers in list
    product :: Num a=> [a] -> a - product of numbers in list
    and :: [Bool] -> Bool - conjunction of booleans
    or :: [Bool] -> Bool - дизюнкция of booleans
    concat :: [[a]] -> [a] - concatenation of list of lists
-}

{- Lambda functions
    \{<Parameter>} -> <Body>
    \ <parameter1> ... <parameterN> -> <Body>

    Examples:
    id = \x -> x
    const = \x y -> x
    (\x -> 2 * x + 1) 3 -> 7
    (\x l -> l ++ [x]) 4 [1..3] -> [1,2,3,4]
    (\(x,y) -> x^2 + y) (3,5) -> 14
    (\f x -> f (f x)) (*3) 4 -> 36
-}

{- Functions
    
    Map:
    map :: (a -> b) -> [a] -> [b]

    Filter:
    filter :: (a -> Bool) -> [a] -> [a]
-}