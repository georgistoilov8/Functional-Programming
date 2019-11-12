#lang racket

;;;  Структури от данни в Scheme

;;; Матрици

;;; Представяне на матрица:
;;; ( (1 2 3) (4 5 6) ) е ( 1 2 3 )
;;;                       ( 3 4 5 )

;;; Проверка за коректност на матрица
(define (all? p? l)
  (foldr (lambda (x y) (and x y)) #t (map p? l)))

(define (matrix? m)
  (and (list? m)
       (not (null? (car m)))))

;;; Базови операции с матрици

;;; Брой редове
(define (get-rows m) (length m))
;(get-rows '( (1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;;; Брой стълбове
(define (get-columns m)
  (if (null? m) 0
      (length (car m))))
;(get-columns '( (1 2) (3 4) (5 6) (7 8)))

;;; Намиране на първи ред
(define (first-row m)
  (if (null? m) m
      (car m)))
;(first-row '( (1 2) (3 4) (5 6) (7 8)))
;;; Съкратен вариант
(define get-first-row car)


;;; Намиране на първа колона
(define (first-column m)
  (if (null? m) m
      (if (null? (car m)) '()
                 (cons (car (car m)) (first-column (cdr m))))))
;(first-column '( (1 2) (3 4) (5 6) (7 8)))
;;; Съкратен вариант(за досетливите)
(define (get-first-column m) (map car m))

;;; Изтриване на първи ред
(define remove-first-row cdr)
;(remove-first-row '( (1 2) (3 4) (5 6) (7 8)))

;;; Изтриване на първа колона
(define (remove-first-column m) (map cdr m))
;(remove-first-column '( (1 2) (3 4) (5 6) (7 8)))

;;; Намиране на ред по индекс
(define (get-row index m) (list-ref m index))
;(get-row 2 '( (1 2) (3 4) (5 6) (7 8)))

;;; Намиране на стълб по индекс
(define (get-column index m)
  (map (lambda (row) (list-ref row index)) m))
;(get-column 1 '( (1 2) (3 4) (5 6) (7 8)))

;;; Транспониране на матрица
;;; Вариант 1(директна рекурсия
(define (transpose m)
  (if (null? (get-first-row m)) '()
      (cons (first-column m) (transpose (remove-first-column m)))))
;(transpose '( (1 2) (3 4) (5 6) (7 8)))

;;; Вариант 2(accumulate)
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (transpose2 m)
  (accumulate cons '() 0 (- (get-columns m) 1) (lambda (index) (get-column index m)) (lambda (x) (+ x 1))))
;(transpose2 '( (1 2) (3 4) (5 6) (7 8)))

;;; Аритметични операции над матрици
; Събиране на матрици
(define (sum-vectors v1 v2) (map + v1 v2))
(define (sum-matrices m1 m2) (map sum-vectors m1 m2))
;(sum-matrices '((1 2) (3 4)) '((5 6) (7 8)))

; Умножение на матрици
(define (mult-vectors v1 v2) (apply + (map * v1 v2)))
(define (mult-matrices m1 m2)
  (let ((m2t (transpose m2)))
    (map (lambda (row)
           (map (lambda (column) (mult-vectors row column)) m2t))
         m1)))
;(mult-matrices '( (5 8 -4) (6 9 -5) (4 7 -2)) '( (2) (-3) (1)))
