;;; Функции с произволен брой аргументи
;;; (lambda <списък> <тяло>)
;;; създава функция с <тяло>, която получава <списък> от параметри

;;; (lambda ({<параметър>}+ . <списък>) <тяло>)
;;; Създава функция с <тяло>, която получава няколко задължителни
;;; <параметър> и <списък> от опционални параметри

;;; (define (<функция>.<списък>) <тяло>) <=> (define <функция> (lambda <списък> <тяло>))
;;; (дефине (<функция> {<параметър>}+.<списък>) <тяло>) <=> (define <функция> (lambda ({<параметър>}+.<списък>) <тяло>))

;;; Примери:
(define (maximum x . l) (foldl max (cons x l)))
(maximum 7 3 10 2) ; 10
(maximum 100) ; 100
(maximum) ; Грешка!
(define (g x y . l) (append x l y l))
(g '(1 2 3) '(4 5 6))) ; (1 2 3 4 5 6)
(g '(1 2 3) '(4 5 6) 7 8)) ; (1 2 3 7 8 4 5 6 7 8)

;;; map също е n-местна функция
(map + '(1 2 3) '(4 5 6))
(map list '(1 2 3) '(4 5 6))
(map fold (list * +) '(1 0) '((1 2 3) (4 5 6)))


;;; Функция apply
;;; (Прилагане на функция над списък от параметри)
;;; (apply <функция> <списък>)
;;; Прилага <функция> над <списък> от параметри
;;; Примери:
(apply + '(1 2 3 4 5))
(apply append '((1 2) (3 4) (5 6)))
(apply list '(1 2 3 4))

;;; Оценяване на списък като комбинация (eval)
;;; (eval <списък> <среда>)
;;; Връща оценката на оценката на <списък> в <среда>
;;; (interaction-environment) — текущата среда, в която оценяваме
(define (evali x) (eval x (interaction-environment)))