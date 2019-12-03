{-
    Functions
-}

{-
    Case
    <name> {<argument>}
        { | <keeper> = <expression> }
    General:
    <name> <argument1> <argument2> ... <argumentN>
        | <keeper1> = <expression1>
        | <keeper2> = <expression2>
        ...
        | <keeperN> = <expressionN>
    If <keeper1> is True => return <expression1>, else
        ...
    If <keeperN> is Ture => retunr <expressionN> else Error
-}

fact n 
  | n == 0 = 1
  | n > 0  = n * fact (n - 1)
  | n < 0 = error "Negative number given"
-- Question: fact (-5) -> ? => Error, so we add n < 0
fib n
  | n == 0 = 1
  | n == 1 = 1
  | n > 1 = fib(n-1) + fib(n-2)
  | n < 0 = error "Negative number given"

get_grade x
  | x >= 5.5 = "Отличен"
  | x >= 4.5 = "Много Добър"
  | x >= 3.5 = "Добър"
  | x >= 3 = "Среден"
  | otherwise  = "Слаб"

{-
    Local definitions with let:
    let {<defition>} in <body>
    let <definition1> <defintion2> ... <definitionN> in <body>
-}
-- Examples: 
-- let x = 5 in x + 3
{- let f x = y + x
    y = 7
    in f 2 * y
-}

fact2 n = let fact n = if n == 0 then 1 else n * fact(n-1)
          in (fact n)^2

{-
    Local definition with where
    <definition-of-function>
        where {<definition>}
    <definition-of-function>
        where <definition1>
              <definition2>
              ...
              <definitionN>
-}

-- Examples:
sumLastDigits n = lastDigit n + lastDigit (stringDigit n)
  where lastDigit = (`mod` 10)
        stringDigit = (`div` 10)

quadratic a b c
  | a == 0 = "Linear equation"
  | d > 0 = "Has two roots"
  | d == 0 = "Has only one root"
  | otherwise = "Has no roots"
  where d = b*b - 4*a*c