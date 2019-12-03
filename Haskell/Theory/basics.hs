{-
    Haskell - The new beginning
-}

{-
    Identificators: georgi, b, _asd
    Name of objecst starts with lower case letter or _.
    Also, UTF-8 is supported.
-}

{-
    Reserved identificators: case, if, let, where, ...
-}

{-
    Constructors: Integer, Maybe, Just, ...
    Names of constructors start with upper case letter
-}

{-
    Numbers: 10, -5.12, 3.2e+2, 1.2E-2, 0x2f, 0o35
-}

{-
    Operations: +, *, &%, <==>, (emoji)
-}

{-
    Reserved operations: .. : :: = \| <- -> @ => ~
-}

{-
    Special symbols: () , ; [] ' {}
    Symbols: 'a', '\n', '+'
    Strings: "Hello, World!", "Само Левски"
-}

{-
    Declaration and definition
    <name> :: <type>. We declare, that <name> will be connected
    to the value of <type>
    Declarations are not mandatory. Most of the time Haskell
    can find out what type to use.

    <name> = <expression> (definition)
-}
-- Example:
x :: Int        -- declaration
y :: Double     -- declaration
z :: String     -- declaration
x = 2                       -- definition
y = fromIntegral x^2 + 7.5  -- definition
z = "Hello, Georgi"         -- definition
-- z = x + y -> Error

{-
    Types:
    Bool - True or False
    Char - Unicode symbols
    Int, Integer
    Float, Double
    Composite types: [a] - list with random length of elements of type a
    -- Example: String = [Char]
    (a, b, c) - list with fixed length of elements all from different types
-}

{-
    Modules:
    module <name> where -> defines module with <name>
    import <module> [(<name>{, <name>})] -> imports all definitions of <name> from <module>
-}

{-
    Arithmetic operations: +, -, *, /, ^, ^^
    More numeric functions: div, mod, max, min, gcd, lcm
    Cast functions: fromIntegral, fromInteger, toInteger,
        realToFrac, fromRational, toRational, round, ceiling floor
    Functions on rational numbers: exp, log, sin, cos, tan, asin, acos, atan, sqrt, **
-}

{-
    Standart predicates: <, >, ==, /=, <=, >=, odd, even
    Boolean operations: &&, ||, not
-}

{-
    Functions: 
    type1 -> type2 - Type of function which recieves agrument of
        <type1> and returns object of type <type2>
    <name> <argument> = <body> - Definition of a function
    with <name>, one <argument> and <body>
    <function> <expression>
-}
-- Example:
square :: Int -> Int
square x = x * x
-- square 2 -> 4
-- square 2.7 -> Error (Since Haskell does not auto cast like C++, we can 
--                      not give agrument of type Double as an argument of type Int)

-- Functions are with higher priority than arithmetic functions:
-- square 2 + 3 -> 7
-- square (2 + 3) -> 25

-- !! Functions in Haskell have only 1 argument !!

{-
    How to have functions with many parameters?
    Simply: t1 -> (t2 -> t3) - This is a function, which
    gets an argument of type <t1> and returns function, which
    gets an argument of type <t2> and returns object of type <t3>
    OR
    This is a function with two arguments of <t1> and <t2>, 
    which returns object of type <t3>.
    
    General case: <function> :: t1 -> (t2 -> (t3 -> ... (tn -> t) ... ))
    <function> <argument1> ... <argumentN> = <body>
-}
-- Example:
hypothenuse :: Double -> Double ->Double
hypothenuse a b = sqrt(a**2 + b**2)

-- !! t1 -> (t2 -> t3) != (t1 -> t2) -> t3

{-
    High-order functions
-}
twice f x = f(f x)
twice :: (Int -> Int) -> Int -> Int
-- twice square 3 -> 81
-- twice (mod 13) 5 -> 1

diag f x = f x x
diag :: (Int -> Int -> Int) -> Int -> Int

{-
    Functions in Haskell are always with prefix order
    Operations in Haskell are all binary infix order
    With '<function>' we can make binary function
-}
-- Example:
-- max 2 3 -> 3
-- 2 `max` 3 -> 3

-- Change operation into two argument functions:
-- (+) 2 3 -> 5
-- plus1 = (+) 1
-- square = diag (*)

-- Change operations into one argument functions:
-- (<expression> <operation>) -> left cut
-- (<operation> <expression>) -> right cut
-- (2^) 3 -> 8
-- (^2) 3 -> 9
-- lastDigit = (`mod` 10)

{-
    if ... then ... else
    if <condition> then <expression1> else <expression2>
    If <condition> is True => we return <expression1>
    If <condition> is False => we return <expression2>
-}
-- Examples:
abs x = if x < 0 then -x else x
fact n = if x == 0 then 1 else n * fact(n-1)
-- if x > 5 then x + 2 else "Error" -> Error
-- <expression1> and <expression2> must be the same type
-- <condition> must be Bool
