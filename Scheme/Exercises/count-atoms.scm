;;; Дълбоки списъци
;;; Пример: ((1 (2)) (((3) 4) (5 (6)) () (7)) 8)
;;; Задача: Да се преброят атомите в дълбокия списък

(define (atom? x) (and (not (null? x)) (not (pair? x))))

(define (count-atoms l)
  ( cond ( (null? l) 0)
         ( (atom? l) 1)
         ( else (+ (count-atoms (car l)) (count-atoms (cdr l))))))
