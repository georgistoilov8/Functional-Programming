;;; Recursive functions

;;; Factoriel
(define (fact n)
  (if (zero? n) 1 (* n (fact (- n 1)))))

(fact 4)
(fact 10)

;;; (define (f x) (+ 1 (f (- x 1))))
;;; (f 0) - безкраен цикъл

;;; Factoriel with loop
(define (for i n)
  (if (= i n) n
  (* i (for (+ 1 i) n))))

;;; Also works
(define (for2 n r i)
  (if (<= i n)
      (for n (* r i) (+ i 1))
      r))

(define (fact2 n)
  (for 1 n))

(fact2 4)

;;; Nested Definition
;;; (define (<function> <parameter>) <definition> <body>)
;;; При извикване на <функция> първо се оценяват всички <дефиниция> и след
;;; това <тяло>
;;; Example:
(define (dist x1 y1 x2 y2)
  (define dx (- x2 x1))
  (define dy (- y2 y1))
  (define (sq x) (* x x))
  (sqrt (+ (sq dx) (sq dy))))
(dist 0 0 4 3)

;;; Можем да променим малко предходната итеративна функция за пресмятане на факториел
(define (fact n)
  (define (for r i)
    (if (<= i n)
        (for (* r i) (+ i 1))
        r))
  (for 1 1))

(fact 4)

;;; Специална форма let
;;; (let ({(<символ> <израз>)}) <тяло> )
;;; (let ((<символ1> <израз1>)
;;;       (<символ2> <израз2>)
;;;       ...
;;;       (<символN> <изразN>)) <тяло>)
(define (sq x) (* x x))
(define (dist x1 y1 x2 y2)
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (sqrt (+ (sq dx) (sq dy)))))
(dist 0 0 3 4)

#| Ще даде грешка тъй като (p (/ (+ a b c) 2)) няма да може да бъде оценене, Защото няма оценки на a,b,c
(define (area x1 y1 x2 y2 x3 y3)
  (let ((a (dist x1 y1 x2 y2))
        (b (dist x2 y2 x3 y3))
        (c (dist x3 y3 x1 y1))
        (p (/ (+ a b c) 2)))
    (sqrt (* p (- p a) (- p b) (-p c)))))
|#

;;; Специална форма let*
#|
    (let* ((<символ1> <израз1>
           (<символ2> <израз2>
           ...
           (<символN> <изразN>)) <тяло>)
|#
;;; С let* вече можем да направим горния пример
(define (area x1 y1 x2 y2 x3 y3)
  (let* ((a (dist x1 y1 x2 y2))
        (b (dist x2 y2 x3 y3))
        (c (dist x3 y3 x1 y1))
        (p (/ (+ a b c) 2)))
    (sqrt (* p (- p a) (- p b) (- p c)))))
(area 0 0 2 3 1 10)

;;; Степенуване (power)
(define (pow x n)
  (cond ( (= n 0) 1)
        ( (< n 0) (/ 1 (pow x (- n))))
        ( else (* x (pow x (- n 1))))))
(pow 2 4)

;;; Бързо степенуване
(define (fast-pow x n)
  (cond ( (= n 0) 1)
        ( (< n 0) (/ 1 (fast-pow x (- n))))
        ( else (if (even? n) (fast-pos (fast-pow x (/ n 2)) 2) (* x (fast-pow x (- n 1)))))))

#|
Алтернативна дефиниция
(define (qpow x n) (define (sqr x) (* x x)) (cond ((= n 0) 1) ((< n 0) (/ 1 (qpow x (- n)))) ((even? n) (sqr (qpow x (quotient n 2)))) (else (* x (qpow x (- n 1))))))
|#

(pow 2 64)

;;; Числа на Фибоначи
;;; Бавен метод. При 35-тото число се замисля, а при 40-тото? Там трудно ще върне отговор
(define (fib n)
  (if (or (= n 0) (= n 1)) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(fib 35)
;(fib 40)

;;; Можем да оптимизираме като помним последните две числа на Фибоначи
;;; и генерираме последователно всички такива числа
(define (fib n)
  (define (iter i fi fi-1)
    (if (= i n) fi
        (iter (+ i 1) (+ fi fi-1) fi)))
  (if (= n 0) 0 (iter 1 1 0)))
(fib 40)
(fib 1000)