;;; Basics of Scheme

(define endOfParagraph "@---------------@")
(define newL "#\newline")

;;; #f - False, #t - True

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

(newLine)
endOfParagraph
(newLine)

;;; Стандартни предикати(Standard Predicates)
"Predicates for number comparison"
(< 2 3) ;;; return #t (2 < 3)
(> 2 3) ;;; return #f (2 > 3)
(= 2 2) ;;; return #t (2 =? 2)
(= 2 3) ;;; return #f (2 =? 3)
(<= 2 2) ;;; return #t (2 <= 2)
(>= 2 3) ;;; return #f (2 >= 3)

"Number predicates"
(zero? 2) ;;; return #f
(zero? 0) ;;; return #t
(negative? -5) ;;; return #t
(positive? -5) ;;; return #f
(odd? 4) ;;; #f
(even? 4) ;;; #t

"Predicates for checking a type"
(boolean? #f) ;;; return #t
(boolean? 5) ;;; return #f
(number? 5) ;;; return #t
(char? #\space) ;;; return #t
(string? "Hello") ;;; return #t
(symbol? '+) ;;; return #t
(procedure? +) ;;; return #t

(newLine)
endOfParagraph
(newLine)

;;; Условна форма if
;;; (if <условие> <израз1> <израз2>)
;;; Оценява се <условие>. Ако оценката е #t, то се връща оценката на <израз1>
;;; Ако оценката е #f, то се връща оценката на <израз2>

"If statement"
(if (< 3 5) (+ 3 5) (* 3 5)) ;;; return 8 = 3 + 5, 3 < 5 - #t

(define (absN x) (if (< x 0) (- x) x))
(absN 5) ;;; return 5
(absN -5) ;;; return 5

(define (f x) (if (< x 5) (+ x 2) "Error"))
(f 3) ;;; return 5
(f 5) ;;; return "Error"

(define (g x y) (if (< x y) (+ x y) (* x y)))
(define (g x y) ((if (< x y) + *) x y))
(g 2 3) ;;; return 5
(g 3 2) ;;; return 6

(newLine)
endOfParagraph
(newLine)

#|
;;; Форма за многозначен избор cond
;;; (cond (<условие1> <израз1>)
;;;       (<условие2> <израз2>)
          ...
          (<условиеN> <изразN>)
          (else <изразN+1>
Ако <условие1> е оценено до #t се връща <израз1>, иначе ако <условие2> е
оценено до #t се връща <израз2>, иначе и т.н. Ако всички са оценени до #f,
то се връша израза след else(<изразN+1>) 
|#
(define (grade x)
  (cond ((> x 5.5) "Отличен")
        ((> x 4.5) "Много Добър")
        ((> x 3.5) "Добър")
        ((> x 3.0) "Среден")
        (else "Слаб")))
(grade 5.64) ;;; return "Отличен"
(grade 4.90) ;;; return "Много Добър"
(grade 4.00) ;;; return "Добър"
(grade 3.23) ;;; return "Среден"
(grade 2.00) ;;; return "Слаб"

(newLine)
endOfParagraph
(newLine)

#| Форма за разглеждане на случаи case
(case <тест> ((<случай1,1> ... <случай1,к1>) <израз1>)
             ((<случай2,1> ... <случай2,к2>) <израз2>)
             ...
             ((<случайN,1> ... <случайN,кN>) <изразN>
             ( else <изразN+1>))
|#

(define (divisible? n x)
  (zero? (remainder n x)))

(define (leap? year) (if (and (divisible? year 4) (not (divisible? year 100))) #t (if (divisible? year 400) #t #f)))

(define (days-in-month m y)
  (case m
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (else (if (leap? y) 28 29))))
(days-in-month 2 2004) ;;; return 28
(days-in-month 2 2003) ;;; return 29

(newLine)
endOfParagraph
(newLine)

;;; Логически операции
;;; (not <булев-израз>) - Връща отрицанието на <булев-израз>

;;; (and <булев-израз1> <булев-израз2> ... <булев-изразN>) - Оценява последователно всеки
;;; <булев-израз>. Ако всички връщат #t => и and връща #t. Ако поне един от тях
;;; върне #f => and връща #f и не оценява останалите <булев-израз>

;;; (or <булев-израз1> <булев-израз2> ... <булев-изразN>) - Оценява последователно всички
;;; <булев-израз>. Ако всички се оценяват до #f, or връща #f. Ако поне един от тях се оцени
;;; до #t, то or се оценява до #t и не се оценяват останалите <булев-израз>

;;; (not x) <=> (if x #f #t)
;;; (and x y) <=> (if x y #f)
;;; (or x y) <=> (if x #t y)