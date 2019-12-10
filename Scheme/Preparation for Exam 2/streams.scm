#lang racket

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))

(define head car)
(define (tail s) (force (cdr s)))
(define (take n stream)
  (if (= n 0) '()
      (cons (head stream) (take (- n 1) (tail stream)))))

(define ones (cons-stream 1 ones))
(define (from n) (cons-stream n (from (+ n 1))))
(define nats (from 0))

(define (iterate f x) (cons-stream x (iterate f (f x))))
(define squares (iterate (lambda (x) (* x x)) 2))

(define (map-stream f stream)
  (cons-stream (f (head stream)) (map-stream f (tail stream))))

(define (filter-stream p? stream)
  (if (p? (head stream)) (cons-stream (head stream) (filter-stream p? (tail stream)))
      (filter-stream p? (tail stream))))

(define (zip-streams op stream1 stream2)
  (cons-stream (op (head stream1) (head stream2))
               (zip-streams op (tail stream1) (tail stream2))))

(define squares2 (map-stream (lambda (x) (* x x)) nats))
(define odd (filter-stream odd? nats))
(define 1+ (zip-streams + nats ones))