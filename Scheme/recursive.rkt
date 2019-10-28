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
