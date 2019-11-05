;;; Да се напише функция (meetTwice? f g a b), която проверява дали
;;; в целочисления интервал [a, b] съществуват две различни цели числа
;;; х и у такива, че f(x) = g(x) и f(y) = g(y)

;;; Пример: (meetTwice? (lambda(x)x) (lambda(x) (- x)) -3 1) → #f
;;;         (meetTwice? (lambda(x)x) sqrt 0 5) → #t

(define (doesMeet? f g n)
  (if (= (f n) (g n)) 1 0))

(define (check f g a b)
  (if (> a b) 0
      (+ (doesMeet? f g a) (check f g (+ a 1) b))))

(define (meetTwice? f g a b)
  (if (< (check f g a b) 2) #f #t))