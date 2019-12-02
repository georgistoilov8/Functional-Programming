#lang racket

;;; Абстракция със структури от данни
;; Рационално число

; Базови операции
;(define (make-rat n d) (cons n d))
;(define make-rat cons)
;(define (get-numer r) (car r))
;(define get-numer car)
;(define (get-denom r) (cdr r))
;(define get-denom cdr)

; По-добре:
(define (make-rat2 n d)
  (if (= d 0) (cons n 1) (cons n d)))

; Още по-добре. Работим с нормализирани дроби
; Дефинирана по-надолу по още един начин
#|(define (make-rat n d)
  (if (or (= d 0) (= n 0)) (cons 0 1)
      (let* ( (g (gcd n d))
              (ng (quotient n g))
              (dg (quotient d g)))
        (if (> dg 0) (cons ng dg)
            (cons (- ng) (- dg))))))
|#
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

;;; До тук не може да се различи сигнатурата на структурата. Освен рационални числа,
;;; с две числа се представя точка в пространството, комплексно число и т.н.

;<-------------------------------
; Нека добавим етикет на обекта
#|(define (make-rat n d)
  (cons 'rat
        (if (or (= d 0) (= n 0)) (cons 0 1)
            (let* ( (g (gcd n d))
                    (ng (quotient n g))
                    (dg (quotient d g)))
              (if (> dg 0) (cons ng dg)
                  (cons (- ng) (- dg)))))))
|#
;(define get-numer cadr)
;(define get-denom cddr)

; Вече може да се направи проверка дали даден обект е рационално число
(define (rat? p)
  (and (pair? p) (eqv? (car p) 'rat)
       (pair? (cdr p))
       (integer? (cadr p)) (positive? (cddr p))
       (= (gcd (cadr p) (cddr p)) 1)))

; Можем да добавим проверка за коректност:
(define (check-rat f)
  (lambda (p)
    (if (rat? p) (f p) 'error)))

(define get-numer (check-rat cadr))
(define get-denom (check-rat cddr))
;------------------------------------------>

;<------------------------------------------
; Операциите над структурата от данни са видими глобално, но можем да ги направим 'private'
(define (make-rat n d)
  (let* ((g (gcd n d))
         (numer (quotient n g))
         (denom (quotient d g)))
  (lambda (prop . params)
    (case prop
      ('get-numer numer)
      ('get-denom denom)
      ('print (cons numer denom))
      ('* (let ((r (car params)))
            (make-rat (* numer (r 'get-numer))
                      (* denom (r 'get-denom)))))
      (else 'unknown-prop)))))
;(define r (make-rat 4 6); (r 'print) -> (2 . 3)
;(define r1 (make-rat 3 5)); (define r2 (make-rat 5 2)); ((r1 '* r2) 'print) -> '(3 . 2)

#|
(define (make-rat n d)
  (let* ((g (gcd n d))
         (numer (quotient n g))
         (denom (quotient d g)))
  (define (self prop . params)
    (case prop
      ('get-numer numer)
      ('get-denom denom)
      ('print (cons numer denom))
      ('* (let ((r (car params)))
            (make-rat (* numer (self 'get-numer))
                      (* denom (self 'get-denom)))))
      (else 'unknown-prop)))
self))
|#