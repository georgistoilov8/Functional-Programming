;;; Функции от по-висок ред
;;; Функция, която приема функция за параметър се нарича функция от
;;; по-висок ред.

;;; Задачи за сумиране
;;; k^2 + (k+1)^2 + ... + 100^2 за k <= 100
(define (sq x) (* x x))

(define (sum k)
  (if (> k 100) 0 (+ (* k k) (sum (+ k 1)))))
(sum 99)

(define (sum2 a b f dx)
  (if (> a b) 0 (+ (* dx (f a)) (sum2 (+ a dx) b f dx))))

(define (sum3 x)
  (if (> x (expt 10 1000)) 0 (+ x (sum3 (exp x)))))

;;; Функция от по-висок ред sum, която пресмята Сума от temr(i)
(define (sum a b term next)
  (if (> a b) 0 (+ (term a) (sum (next a) b term next))))

;;; Сума на квадратите на числата от к до 100
(define (square x) (* x x))
(define (1+ x) (+ x 1))
(define (sum1 k) (sum k 100 square 1+))
(sum1 99)

;;; Интеграл
(define (sum2 a b f dx)
  (define (term x) (* dx (f x)))
  (define (next x) (+ x dx))
  (sum a b term next))

;;; Сума от е^x ...
(define (id x) x)
(define (sum3 x)
  (sum x (expt 10 1000) id exp))
(sum3 10)


;;; Функция от по-висок ред за умножение
(define (product a b term next)
  (if (> a b) 1 (* (term a) (product (next a) b term next))))


;;; Обща функция за натрупване
(define (accumulate operation null-value a b term next)
  (if (> a b) null-value
      (operation (term a) (accumulate operation null-value (next a) b term next))))

;;; Сега вече можем да напишем функциите за сумиране и умножение по следния начин:
(define (sum a b term next) (accumulate + 0 a b term next))
(sum 1 10 id 1+)

(define (prod a b term next) (accumulate * 1 a b term next))
(prod 1 10 id 1+)

;;; Стойност на полином
(define (findP n x)
  (define (term i) (*(- (1+ n) i) (expt x i)))
  (accumulate + 0 0 n term 1+))
(findP 4 2)

;;; Решение №2
;;; Ползваме правило на Хорнер
;;; Операция: ux+v
;;; Смята грешно
(define (findP n x)
  (define (op u v) (+ (* u x) v))
  (accumulate op 0 1 (1+ n) id 1+))
(findP 4 2)

;;; Операцаия: u+vx
;;; И това смята грешно
(define (findP n x)
  (define (op u v) (+ u (* v x)))
  (accumulate op 0 1 (1+ n) id 1+))
(findP 4 2)

;;; Функция, която пресмята ляво натрупване
(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))
;;; accumulate - дясно натрупване, рекурсивен процес
;;; accumulate-i - ляво натрупване, итеративен процес

;;; Крайно решение на задачата
(define (findP n x)
  (define (op u v) (+ (* u x) v))
  (accumulate-i op 0 1 (1+ n) id 1+))
(findP 4 2)