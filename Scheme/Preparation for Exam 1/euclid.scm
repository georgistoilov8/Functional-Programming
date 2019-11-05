;;; Да се реализира алгоритъмът на Евклид за намиране на най-голям общ
;;; делител.
(define (gcd2 n m)
  (let ( (bigNumber (max n m))
         (smallNumber (min n m)))
  (if (= (remainder bigNumber smallNumber) 0) smallNumber
      (gcd smallNumber (remainder bigNumber smallNumber)))))