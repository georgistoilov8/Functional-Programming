#lang racket

;;; Абстракция със структури от данни
;; Рационално число

; Базови операции
;(define (make-rat n d) (cons n d))
;(define make-rat cons)
(define (get-numer r) (car r))
;(define get-numer car)
(define (get-denom r) (cdr r))
;(define get-denom cdr)

; По-добре:
(define (make-rat2 n d)
  (if (= d 0) (cons n 1) (cons n d)))

; Още по-добре. Работим с нормализирани дроби
(define (make-rat n d)
  (if (or (= d 0) (= n 0)) (cons 0 1)
      (let* ( (g (gcd n d))
              (ng (quotient n g))
              (dg (quotient d g)))
        (if (> dg 0) (cons ng dg)
            (cons (- ng) (- dg))))))
; Аритметични операции
; Умножение на рационални числа
(define (*rat r1 r2) (make-rat (* (get-numer r1) (get-numer r2))
                                (* (get-denom r1) (get-denom r2))))
;(*rat (make-rat2 1 2) (make-rat2 2 3))

; Събиране на рационални числа
(define (+rat p q) (make-rat
                    (+ (* (get-numer p) (get-denom q))
                       (* (get-denom p) (get-numer q)))
                    (* (get-denom p) (get-denom q))))
;(+rat (make-rat2 1 2) (make-rat2 2 3))

; Сравнение на рационални числа
(define (<rat p q)
  (< (* (get-numer p) (get-denom q))
     (* (get-numer q) (get-denom p)))) 
;(<rat (make-rat2 1 2) (make-rat2 2 3))

;;; Програми с рационални числа
; Намиране на експонента
(define (pow x n)
  (cond ( (= n 0) 1)
        ( (< n 0) (/ 1 (pow x (- n))))
        ( else (* x (pow x (- n 1))))))
(define (fact x)
  (if (= x 0) 1
      (* x (fact (- x 1)))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (my-exp x n)
  (accumulate +rat (make-rat 0 1) 0 n (lambda (i) (make-rat (pow x i) (fact i))) (lambda (x) (+ x 1))))

;;; Coming soon more 