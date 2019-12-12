#lang racket
(require rackunit rackunit/text-ui)

(define (prefixes xs)
  (define (prefixes-helper l current result)
    (if (null? l) (append result (list current))
        (prefixes-helper (cdr l) (append current (list (car l))) (append result (list current)))))
  (prefixes-helper xs '() '()))

(define prefixes-test
  (test-suite "Prefixes tests"
              (test-case "Should return '(()) for list '()" (check-equal? (prefixes '()) '(()) ))
              (test-case "Should return '(() (1)) for list '(1)" (check-equal? (prefixes '(1)) '(() (1)) ))
              (test-case "Should return '(() (1) (1 2)) for list '(1 2)" (check-equal? (prefixes '(1 2)) '(() (1) (1 2)) ))
              (test-case "Should return '(() (3) (3 5) (3 5 7) (3 5 7 1)) for list '(3 5 7 1)" (check-equal? (prefixes '(3 5 7 1)) '(() (3) (3 5) (3 5 7) (3 5 7 1)) ))
              ))
(run-tests prefixes-test 'verbose)