;;; Свиване на дълбоки списъци
;;; (deep-foldr <x-дъно> <в-дъно> <операция> <списък>)
(define (deep-foldr nv term op l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        ( else (op (deep-foldr nv term op (car l))
                   (deep-foldr nv term op (cdr l))))))

(define (count-atoms l) (deep-foldr 0 (lambda (x) 1) + l))
(count-atoms '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

;;; Вместо lambda може и само list
(define (flatten l) (deep-foldr '() (lambda (x) (list x)) append l))
(flatten '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

(define (snoc x l) (append l (list x)))
(define (deep-reverse l) (deep-foldr '() (lambda (x) x) snoc l))
