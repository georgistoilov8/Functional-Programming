;;; Подаване на функции като параметри

(define (fixed-point? f x) (= (f x) x))
(fixed-point? sin 0)
(fixed-point? exp 1)
;(fixed-point? expt 0) ;;; Грешка

(define (fact x)
  (if (= x 0) 1
      (* x (fact (- x 1)))))

(define (branch p? f g x) ( (if (p? x) f g) x))
(branch odd? exp fact 4)
(branch even? exp fact 6)

(define (id x) x)
(branch number? log id "1")
(branch number? log id 1)

(branch string? number? procedure? symbol?)
