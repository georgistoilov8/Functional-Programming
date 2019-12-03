#lang racket

; Потоци

; Понякога се налага да работим с тежки операции. Тогава можем да обещаем, че ще свършим
; нещо в някой етап в бъдещето.

; Дефиниция на "Обещание": Функция, която ще изчисли и върне някаква стойност в бъдещ момент
; от изпълнението на програмата. Нарича се още promise и отложена
; операция.

; Примитивни операции force и delay:
; (delay <израз>) -> връща обещание
; (force <обещание>) -> връща оценката на <израз>

; Пример:
(define prom (delay (+ 2 5)))
(force prom)

; (define error (delay (car '())))
; (force error)

; Изразът не се оценява докато не се форсира неговото оценяване.
; Ако имаме следните редове:
(define undefined (delay (+ a 5)))
(define a 3)
(force undefined)
; Това работи, но (define undefined (+ a 5)) няма да работи, защото изразът се оценява веднага,
; а това ще доведе до грешка, тъй като не е дефинирана променливата 'а'.

; !! Обещанията в Scheme мемоизират изчислената стойност !!

; Потоци - Списък, чиито елементи се изчисляват отложено
; (h . t) -> h е произволен елемент, t - обещание

(define the-empty-stream '())
(define (cons-stream h t) (cons h (delay t)))
(define head car)
(define (tail s) (force (cdr s)))
(define empty-stream? null?)

; Дефиниране на специални форми (такива са if, let, let*, case, cond, define, lambda and etc.
; (define-syntax <символ>
;      (syntax-rules () { (<шаблон> <тяло>)})
; Пример:
;(define-syntax cons-stream
;    (syntax-rules () ((cons-stream h t) (cons h (delay t)))))

; Задача:
; Да се построи поток от целите числа в интервала [a, b]
(define (enum a b)
  (if (> a b) the-empty-stream
      (cons-stream a (enum (+ a 1) b))))

; Да се намерят първите n елемента на даден поток
(define (first s n)
  (if (or (empty-stream? s) (= n 0)) '()
      (cons (head s) (first (first (tail s) (- n 1))))))

; Да се намери първата позиция в поток, на която има елемент с дадено свойство:
(define (search-stream p? s)
  (cond ((empty-stream? s) #f)
        ((p? (head s)) s)
        (else (search-stream p? (tail s)))))

; Безкрайни потоци:
(define (from n) (cons-stream n (from (+ n 1))))
;(define nats (from 0))

; Да се генерира поток от числата на Фибоначи:
(define (generate-fibs a b)
  (cons-stream a (generate-fibs b (+ a b))))
;(define fibs (generate-fibs 0 1))

; Трансформиране на безкрайни потоци (map)
(define (map-stream f s) (cons-stream (f (head s))
                                      (map-stream f (tail s))))

; Филтриране на безкрайни потоци (filter)
(define (filter-stream p? s)
  (if (p? (head s))
      (cons-stream (head s) (filter-stream p? (tail s)))
      (filter-stream p? (tail s))))

; Комбиниране на безкрайни потоци (zip)
(define (zip-stream op s1 s2)
  (cons-stream (op (head s1) (head s2))
               (zip-stream (tail s1) (tail s2))))

; map-stream с произволен брой параметри:
(define (map-stream2 f . streams)
  (cons-stream (apply f (map head streams))
               (apply map-stream2 f (map tail streams))))

; Можем да дефинираме потоци чрез директна рекурсия:
;(define ones2 (cons-stream 1 ones2))
;(define nats (cons-stream 0 (zip-streams + ones nats))) 