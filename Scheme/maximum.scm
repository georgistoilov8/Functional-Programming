;;; Да се намери максималният елемент на списък
(define (max_element l)
  (foldr max (car l) l))
