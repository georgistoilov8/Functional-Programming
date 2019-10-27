;;; Basics of Scheme

(define endOfParagraph "@---------------@")
(define newL "#\newline")
;;; (define <символ> <израз>)
;;; define оценява <израз> и свързва <символ> с оценката му
"Define"
(define a 5)
a ;;; return 5

(define b 10)
b ;;; return 10

(+ a b) ;;; return 15

(define helloWorld "Hello, World!")
helloWorld ;;; return "Hello, World!"

(define a (+ 5 b))
a ;;; return 15

;;; Ако някой от изразите не може да се оцени, това води до грешка
#|(define a (* a c))
a
|#

(newLine)
endOfParagraph
(newLine)

;;; Цитиране(quote)
;;; (quote <израз>)
;;; Алтернативен запис: '<израз>
"Quote"
'2 ;;; return 2
'+ ;;; return +
+ ;;; return #<procedure:+>
'(+ 2 3) ;;; return (+ 2 3)
(+ 2 3) ;;; return 6
'(+ 1 '(* 3 4)) ;;; return (+ 1 '(* 3 4))

(newLine)
endOfParagraph
(newLine)

;;; Функция(function)
;;; (define (<function> parameter) <body>)
;;; (define (<функция> параметър) <тяло>)
"Function"
(define (square x) (* x x))
(square 5) ;;; return 25
(square 14)  ;;; return 196

(define (1+ k) (+ 1 k))
(1+ 10) ;;; return 10 + 1 = 11
(square (1+ (square 3))) ;;; return 100

(define (f x y) (+ (square (1+ x)) (square y) 5))
(f 2 4) ;;; return 30

(define (h) (* 2 3))
(h) ;;; return 2*3 = 6
h   ;;; return #<procedure:h>

(newLine)
endOfParagraph
(newLine)

;;; Стандартни числови операции(Standard numeric functions)
;;; Аритметични операции(Arithmetic operations)
"Arithmetic operations"
(+ 1 15) ;;; return 16
(- 16 4) ;;; return 12
(* 6 3) ;;; return 18
(/ 20 10) ;;; return 2

"More numeric functions"
(remainder 5 3) ;;; Връща остатъка при деление на числата 5 и 3. В случая 2. 5 / 3 = 1 и остатък 2
(quotient 10 4) ;;; Връша частното при деление на числата 10 и 4. В слуачая 2. 10 / 4 = 2 и остатък 2
(max 5 10) ;;; return 10
(min 5 10) ;;; return 5
(gcd 12 8) ;;; Най-голям общ делител
(lcm 12 8) ;;; Най-малко общо кратно

"Round functions"
(floor 3.8) ;;; return 3.0
(ceiling 3.2) ;;; return 4.0
(round 3.6) ;;; return 4.0
(round 3.3) ;;; return 3.0

"Additional functions"
(exp 3)
(log 4 2)
(sin 90)
(cos 90)
(tan 90)
(asin 90)
(acos 90)
(atan 90)
(expt 3 4)
(sqrt 256)