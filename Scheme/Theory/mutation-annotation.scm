#lang racket

; Мутиращи операции

; Мутиращите операции в Scheme позволяват въвеждането
; на странични ефекти

; Една мутираща операция е set!
; Промяна на оценка, свързана със символ
; (set! <символ> <израз>)
; Търси се <символ> във веригата от среди
;    Ако е намерен => свързва се с оценката на <израз>
;    Иначе - грешка!
(define a 2)
(set! a 5)
(define (sum x) (set! a (+ a x)) a)
(sum 10)
(sum 10)

(define (make-account sum)
  (lambda (amount)
    (if (< (+ amount sum) 0)
        (display "Insufficient funds!\n")
        (set! sum (+ sum amount)))
    sum))

; Промяна на компоненти: (set-car! <двойка> <израз>), (set-cdr! ,<двойка>, израз)