#lang racket

(append (cons 1 (list '(2) '(3))) (list (cons 4 '())))
; (1 (2) (3) (4))

(define (all? p? l)
  (foldr (lambda (x y) (and x y)) #t (map p? l)))

(define (children v g)
  (cdr (assv v g)))

(define (edge? u v g)
  (memv v (children u g)))

(define (vertices g) (keys g))

(define (keys alist) (map car alist))

(define (parents v g)
(filter (lambda (u) (edge? u v g)) (vertices g)))

(define (euler? g) (all? (lambda (v) (= (length (children v g))
                                        (length (parents v g))))
                         (vertices g)))