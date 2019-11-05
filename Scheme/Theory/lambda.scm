;;; Анонимни функции(lambda)
;;; Конструкция на анонимна функция
;;; (lambda ({<параметър>) <тяло>)
;;; Примери:
(lambda (x) (+ x 3))
((lambda (x) (+ x 3)) 5)

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

;;; (define (<име> <параметри>) <тяло> <=> (define <име> (lambda (<параметри>) <тяло))>
;;; За да работят трябва да се дефинират всички останали функции
(define (integral a b f dx)
  (* dx (accumulate + 0 a b f (lambda (x) (+ x dx)))))

(define (findP n x)
  (accumulate-i (lambda (u v) (+ (* u x) v)) 0 1 (+ n 1) id (lambda (i) (+ i 1))))

;;; n!
(define (fact n)
  (accumulate * 1 1 n (lambda (x) x) (lambda (x) (+ x 1))))
(fact 10)

;;; x^n
(define (expt2 x n)
  (accumulate * 1 1 n (lambda (k) x) (lambda (i) (+ i 1))))
(expt2 2 10)

(define (sum x n)
  (accumulate + 1 0 n (lambda (i) (/ (expt2 x i) (fact i))) (lambda (i) (+ i 1))))

(sum 2 2)


;;; Функции, които връшат функции
(define (square x) (* x x))

(define (twice f x) (f (f x)))
(twice square 3)

(define twice (lambda (f x) (f (f x))))
(twice square 3)
(define twice (lambda (f) (lambda (x) (f (f x)))))
(twice square)
((twice square) 3)

(define (twice f) (lambda (x) (f (f x))))
((twice (twice square)) 2)


(define (n+ n) (lambda (i) (+ i n)))
(define 1+ (n+ 1))
(1+ 7)
(define 5+ (n+ 5))
(5+ 7)

(define (compose f g) (lambda (x) (f (g x))))
((compose square 1+) 3)
((compose 1+ square) 3)
((compose 1+ (compose square (n+ 2))) 3)


;;; Намиране на производна
(define (derive f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))
(define 2* (derive square 0.01))
(2* 5)
((derive (derive (lambda (x) (* x x x)) 0.001) 0.001) 3)

;;; Повторно прилагане на функция
(define (repeated f n)
  (lambda (x) (if (= n 0) x (f ((repeated f (- n 1)) x)))))

(define (repeated f n)
  (if (= n 0) id (compose f (repeated f (- n 1)))))

(define (repeated f n)
  (accumulate compose id 1 n (lambda (i) f) 1+))

;;; N-та производна
(define (derive-n f n dx)
  (if (= n 0) f (derive (derive-n f (- n 1) dx) dx)))

(define (derive-n f n dx)
  ((repeated (lambda (f) (derive f dx)) n) f))

(define (derive-n f n dx)
  ((acumulate compose id 1 n (lambda (i) (lambda (f) (derive f dx))) 1+)f))

;;; Специалната форма lambda е достатъчна за реализация на всички конструкции в Scheme!

;;; (let ((<символ> <израз>)) <тяло>) <=> ((lambda(<символ>) <тяло>) <израз>)
;;; Аналогично и за повече let

;;; Булева логика
;;; Симулация на булеви стойности и if
(define true (lambda (x y) x))
(define false (lambda (x y) y))
(define (lambda-if b x y) ((b x y))) 
;;; Примери
(lambda-if true (lambda () (+ 3 5)) (lambda () (/ 4 0)))
(lambda-if false (lambda () +) (lambda () "abc"))
(define (nott b) (lambda (x y) (b y x)))

;;; Представяне на числа
;;; нумерали на Чърч, представяне на числото като Lf,x f^n(x)
(define c3 (lambda (f x) (f (f (f x)))))
(define c1+ (lambda (a) (lambda (f x) (f (a f x)))))
(define c+ (lambda (a b) (lambda (f x) (a f (b f x)))))

;;; Рекурсивна дефиниция
(define f (gamma f))
;;; Пример:
(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
;;; Ще стане:
(define fact
  (lambda (n)
    (if (= n 0) 1 (* n (fact (- n 1))))))