(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (is-devisible x n)
  (if (= (modulo n x) 0) 1 0))

(define (is-prime? n)
  (if (= (accumulate + 0 2 (- n 1) (lambda (x) (is-devisible x n)) (lambda (x) (+ x 1))) 0) #t #f))

(define (sum-l l)
  (if (null? l) 0
      (+ (car l) (sum-l (cdr l)))))

(define (is-sorted? l)
  (if (or (null? l) (null? (cdr l))) #t
      (and (<= (car l) (car (cdr l))) (is-sorted? (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (insert-sorted l x)
  (cond ((null? l) (list x))
        ((< x (car l)) (cons x l))
        (else (cons (car l) (insert-sorted (cdr l) x)))))

(define (merge l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
        (else (cons (car l2) (merge l1 (cdr l2))))))

(define (filter p? l)
  (cond ( (null? l) l)
        (else (if (p? (car l)) (cons (car l) (filter p? (cdr l))) (filter p? (cdr l))))))


(define (qsort l)
  (if (or (null? l) (null? (cdr l)) l)
      (append (qsort (filter (lambda (x) (< x (car l))) l))
              (filter (lambda (x) (= x (car l))) l)
              (qsort (filter (lambda (x) (> x (car l))) l)))))

(define (flatten l)
  (cond ( (null? l) l)
        ( (list? (car l)) (append (flatten (car l)) (flatten (cdr l))))
        ( else (cons (car l) (flatten (cdr l))))))

(define (depth l)
  (define (depth-help l result)
    (cond ((null? l) result)
          ((list? (car l)) (max (depth-help (car l) (+ result 1))
                                (depth-help (cdr l) result)))
          (else (depth-help (cdr l) result))))
  (depth-help l 1)
  )

(define (sum l)
  (define (sum-help l result)
    (cond ((null? l) result)
          ((list? (car l))
                  (+ (sum-help (car l) 0) (sum-help (cdr l) result)))
          (else (sum-help (cdr l) (+ (car l) result)))))
  (sum-help l 0)
  )

(define (deep-map f l)
  (cond ( (null? l) l)
        ( (list? (car l)) (cons (deep-map f (car l)) (deep-map f (cdr l))))
        ( else (cons (f (car l)) (deep-map f (cdr l))))))


(define (split n)
  (define (split-help n current)
    (cond ( (> current (/ n 2)) n)
          ( (and (integer? (sqrt (- n (* current current)))) (not (= (sqrt (- n (* current current))) 0))) (list current (sqrt (- n (* current current)))))
          (else (split-help n (+ current 1)))))
  (split-help n 1))
    

(define (splitsquares l)
  (cond ( (null? l) l)
        ( (list? (car l)) (cons (splitsquares (car l)) (splitsquares (cdr l))))
        ( else (cons (split (car l)) (splitsquares (cdr l))))))
 