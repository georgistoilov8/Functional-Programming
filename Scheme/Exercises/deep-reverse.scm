;;; Да се обърне редът на атомите в дълбок списък
(define (deep-reverse l)
  (cond ((null? l) l)
        ((atom? l) l)
        ( else (append (deep-reverse (cdr l))
                       (list (deep-reverse (car l)))))))

(deep-reverse '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))
