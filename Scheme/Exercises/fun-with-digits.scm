#lang racket
(require rackunit rackunit/text-ui)

; Домашно, задача 1)'
(define (one . l)
  (if (null? l) 1
      ((car l) 1)))

(define (two . l)
  (if (null? l) 2
      ((car l) 2)))

(define (three . l)
  (if (null? l) 3
      ((car l) 3)))

(define (four . l)
  (if (null? l) 4
      ((car l) 4)))

(define (five . l)
  (if (null? l) 5
      ((car l) 5)))

(define (six . l)
  (if (null? l) 6
      ((car l) 6)))

(define (seven . l)
  (if (null? l) 7
      ((car l) 7)))

(define (eight . l)
  (if (null? l) 8
      ((car l) 8)))

(define (nine . l)
  (if (null? l) 9
      ((car l) 9)))

(define (plus x) (lambda (y) (+ x y)))
(define (minus x) (lambda (y) (- y x)))
(define (times x) (lambda (y) (* x y)))
(define (div x) (lambda (y) (/ y x)))

;;; Unit tests
(define digits-tests
  (test-suite "Digit tests"
              (test-case "Should return 1 for function one" (check-eq? (one) 1))
              (test-case "Should return 2 for function two" (check-eq? (two) 2))
              (test-case "Should return 3 for function three" (check-eq? (three) 3))
              (test-case "Should return 4 for function four" (check-eq? (four) 4))
              (test-case "Should return 5 for function five" (check-eq? (five) 5))
              (test-case "Should return 6 for function six" (check-eq? (six) 6))
              (test-case "Should return 7 for function seven" (check-eq? (seven) 7))
              (test-case "Should return 8 for function eight" (check-eq? (eight) 8))
              (test-case "Should return 9 for function nine" (check-eq? (nine) 9))
              ))
(define operation-tests
  (test-suite "Operation tests"
              (test-case "Should return 5 for function (one (plus (four)))" (check-eq? (one (plus (four))) 5))
              (test-case "Should return 5 for function (four (plus (one)))" (check-eq? (four (plus (one))) 5))
              (test-case "Should return 5 for function (one (plus (two (plus (two)))))" (check-eq? (one (plus (two (plus (two))))) 5))
              (test-case "Should return 5 for function (nine (minus (four)))" (check-eq? (nine (minus (four))) 5))
              (test-case "Should return -5 for function (four (minus (nine)))" (check-eq? (four (minus (nine))) -5))
              (test-case "Should return 24 for function (three (times (eight)))" (check-eq? (three (times (eight))) 24))
              (test-case "Should return 24 for function (eight (times (three)))" (check-eq? (eight (times (three))) 24))
              (test-case "Should return 3 for function (nine (div (three)))" (check-eq? (nine (div (three))) 3))
              (test-case "Should return 4 for function (eight (div (two)))" (check-eq? (eight (div (two))) 4))
              (test-case "Should return 60 for function (one (plus (two (plus (three (plus (four (times (nine (div (three))))))))))) (1 + 2 + 3 + 4*9/3) " (check-eq? (one (plus (two (plus (three (plus (four (times (nine (div (three))))))))))) 18))
              ))
(run-tests digits-tests 'verbose)
(run-tests operation-tests 'verbose)
