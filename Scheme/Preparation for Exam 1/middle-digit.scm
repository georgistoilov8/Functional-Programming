;;; Да се напише функция middle-digit, която намира средната цифра от записа
;;; на подадено естествено число n. Ако n е с четен брой цифри, функцията връща -1

(define (number-length n)
  (if (< n 10) 1
      (+ (number-length (/ n 10)) 1)))

(define (pow n k)
  (if (= k 0) 1
      (* n (pow n (- k 1)))))

(define (middle-digit n)
  (if (even? (number-length n)) -1
      (modulo (floor (/ n (pow 10 (floor ( / (number-length n) 2))))) 10)))