#lang racket

#|
зад.1. Нека е дадено неотрицателно цяло число n. Напишете функция (reduce n), която го "редуцира" до едноцифрено по следната процедура:
- намира най-голямата цифра в числото и я "премахва" от него (при повече от едно срещания премахва най-лявата такава цифра)
- умножава новополученото число по тази премахната цифра и, ако полученото число не е едноцифрено, повтаря процедурата наново за него.
Нека, например, n=26364. Най-голямата цифра е 6 и след премахване на първата шестица получаваме 2364. Умножаваме 2364*6=14184, което още не е едноцифрено, така че продължаваме от това число.
Примери:
(reduce 9) -> 9
(reduce 27) -> 4
(reduce 757) -> 5
(reduce 1234) -> 8
(reduce 26364) -> 8
(reduce 432969) -> 0
(reduce 1234584) -> 8
(reduce 91273716) -> 6
|#

; Намираме най-голямата цифра и нейния индекс в числото
(define (find-largest-digit n)
  (define (find-largest-digit-help n index largest result)
    (if (= n 0) result
        (if (<= largest (modulo n 10))
            (find-largest-digit-help (quotient n 10) (+ index 1) (modulo n 10) (cons (modulo n 10) index))
            (find-largest-digit-help (quotient n 10) (+ index 1) largest result))))
  (find-largest-digit-help n 0 (- 1) '()))

(define (remove-digit number index)
  (+ (* (quotient number (expt 10 (+ index 1))) (expt 10 index))
     (remainder number (expt 10 index))))

(define (reduce n)
  (if (> n 9)
      (let ((largest-index (find-largest-digit n))) 
        (reduce (* (remove-digit n (cdr largest-index)) (car largest-index))))
      n))

#|
Зад.2. Намерете броя на двуцифрените нечетни съставни числа,
които не могат да се представят като сбор на някое просто число и
два пъти по някой точен квадрат (напр. 39 не е такова число, т.к.
може да се представи като 7 + 2*42).
|#
; Проверява дали число е просто
(define (is-prime? number)
  (define half (+ (quotient number 2) 1))
  (define (is-prime-helper number current)
    (if (= half current)
        #t
        (if (= (remainder number current) 0)
            #f
            (is-prime-helper number (+ current 1)))))

   (is-prime-helper number 2))

; Проверява дали число е съставно.
(define (is-constituent? number primes squares)
  (if (null? primes) #f
      (if (odd? (- number (car primes)))
          (is-constituent? number (cdr primes) squares)
          (if (member (quotient (- number (car primes)) 2) squares)
              #t
              (is-constituent? number (cdr primes) squares)))))

; Генерира списък от прости числа, които са по-малки от n
(define (generate-primes n)
  (define (generate-primes-helper n current result)
    (if (= n current) result
        (if (is-prime? current)
            (generate-primes-helper n (+ current 1) (append (list current) result))
            (generate-primes-helper n (+ current 1) result))))
  (generate-primes-helper n 2 '()))

; Създава списък от нечетни числа в интервала [start end]
(define (generate-odd start end)
  (define (generate-odd-helper n current result)
    (if (>= current n) result
        (generate-odd-helper n (+ 2 current) (append (list current) result))))
  (if (odd? start)
      (generate-odd-helper end start '())
      (generate-odd-helper end (+ start 1) '())))

; Създава списък от точните квадрати на числата от 1 до n
(define (generate-squares n)
  (define (generate-squares-helper n current result)
    (if (= n current) result
        (generate-squares-helper n (+ current 1) (append (list (* current current)) result))))
  (generate-squares-helper n 1 '()))

; Връща отговорът на задачата
(define (count-constituent)
  (define primes (generate-primes 100))
  (define odd-numbers (generate-odd 10 100))
  (define squares (generate-squares 10))
  (define (count-constituent-helper primes odd-numbers squares result)
    (if (null? odd-numbers) result
        (if (not (is-constituent? (car odd-numbers) primes squares))
            (count-constituent-helper primes (cdr odd-numbers) squares (+ result 1))
            (count-constituent-helper primes (cdr odd-numbers) squares result))))
  (count-constituent-helper primes odd-numbers squares 0))

; Помощно функция, която не е част от задачата
(define (find-c number primes squares)
  (if (null? primes) 1
      (if (odd? (- number (car primes)))
          (find-c number (cdr primes) squares)
          (if (member (quotient (- number (car primes)) 2) squares)
              (car primes)
              (find-c number (cdr primes) squares)))))

; Използва помощната функция, за да състави списък от нечетно число и съответното просто. Така лесно можем да проверим дали числото е съставно.
; Пример: Връша ((x . y) ...). Съставно е ако следното има точен квадрат: (x - y) / 2
(define (find-all-constituent-numbers)
  (define primes (generate-primes 100))
  (define odd-numbers (generate-odd 10 100))
  (define squares (generate-squares 10))
  (define (count-constituent-helper primes odd-numbers squares result a)
    (if (null? odd-numbers) a
        (if (not (is-constituent? (car odd-numbers) primes squares))
            (count-constituent-helper primes (cdr odd-numbers) squares (+ result 1) a)
            (count-constituent-helper primes (cdr odd-numbers) squares result (append a (list (cons (car odd-numbers) (find-c (car odd-numbers) primes squares))))))))
  (count-constituent-helper primes odd-numbers squares 0 '()))


#|
Зад.3. Напишете функция divisors, която по дадено естествено число
генерира списък от двойки от всичките му прости делители и тяхната
кратност (няма значение в какъв ред). Помнете, че 1 не е просто число,
а и не се брои за делител.
|#
(define (divisors n)
  (define (divisors-help n current count result)
    (if (= n 1)
        (append result (list (cons current count)))
        (if (= (remainder n current) 0)
            (divisors-help (quotient n current) current (+ count 1) result)
            (if (= count 0)
                (divisors-help n (+ current 1) 0 result)
                (divisors-help n (+ current 1) 0 (append result (list (cons current count))))))))
  (divisors-help n 2 0 '()))

#|
Зад.4. Нека е даден списък от числа с дължина 2^n за някое естествено n. Напишете функция fenwick, която построява пълно балансирано двоично дърво с височина n такова, че:
- стойностите в листата са елементите от дадения списък, подредени в същия ред "отляво-надясно" в дървото
- стойността във всеки вътрешен възел е сумата от стойностите на двата му директни наследника. (тоест в корена ще е сумата от всички числа в списъка).
(fenwick '(1 3 5 -1 2 0 -4 3)) ->
          9
    8           1
 4     4     2    -1 
1 3   5 -1  2 0  -4 3
|#

; Дефинираме функцията произволен логаритъм, за да намерим колко дълбок трябва да е списъка
(define logB 
    (lambda (x B) 
      (/ (log x) (log B))))

(define (tree? t)
  (or (null? t)
      (and (list t) (= (length t) 3))
      (tree? (cadr t))
      (tree? (caddr t))))

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define left-tree cadr)
(define right-tree caddr)
(define root-tree car)
(define empty-tree? null?)

(define (depth-tree t)
  (if (empty-tree? t) 0
      (+ 1 (max (depth-tree (left-tree t))
               (depth-tree (right-tree t))))))

; За всеки елемент от списък правим дърво
(define (list-to-leaves l)
  (if (null? l) l
      (cons (make-tree (car l) empty-tree empty-tree) (list-to-leaves (cdr l)))))

; Конструиране на ново ниво в дървото отдолу нагоре.
(define (construct-next-level tree)
  (if (null? tree) tree
      (cons (make-tree (+ (car (car tree)) (root-tree (car (cdr tree)))) (car tree) (car (cdr tree))) (construct-next-level (cdr (cdr tree))))))

(define (fenwick l)
  (define starting-tree (list-to-leaves l))
  (define depth (logB (length l) 2))
  (define (fenwich-helper tree depth-level)
    (if (= depth-level depth) tree
        (fenwich-helper (construct-next-level tree) (+ depth-level 1))))
  (car (fenwich-helper starting-tree 0)))
        

#|
Зад.5. a) Напишете генератор на списъци с подадена дължина и случайни елементи.
b) Напишете функция, която проверява дали списък е сортиран.
c) Напишете няколко (поне 4) алгоритъма за сортиране на списъци и проверете кой е по-бърз (при експерименти с различни размери на списъка). Напишете кратък текст, който описва вашите тестове, наблюдаваните резултати и направете заключения.
Полезен линк: https://docs.racket-lang.org/heresy/random.html
|#

; а)
(define (generate-random-to-N n)
  (define (generate-random-helper n count)
    (if (= count n) (list (random n))
        (cons (random n) (generate-random-helper n (+ count 1)))))
  (generate-random-helper n 1))

(define (generate-random n)
  (define (generate-random-helper n count)
    (if (= count n) (list (random 1 2147483647))
        (cons (random 1 2147483647) (generate-random-helper n (+ count 1)))))
  (generate-random-helper n 1))


; б)
(define (is-sorted? l)
  (cond ( (or (null? l) (null? (cdr l))) #t)
        ( (> (car l) (car (cdr l))) #f)
        ( else (is-sorted? (cdr l)))))

; в)
; Merge Sort
(define (merge list1 list2)
  (cond ( (null? list1) list2)
        ( (null? list2) list1)
        ( (<= (car list1) (car list2)) (cons (car list1) (merge (cdr list1) list2)))
        ( else (cons (car list2) (merge list1 (cdr list2))))))

(define (merge-sort l)
  (if (or (null? l) (null? (cdr l))) l
      (let ((len (quotient (length l) 2)))
            (merge (merge-sort (list-tail (reverse l) (- (length l) len))) (merge-sort (list-tail l len))))))


; Quicksort
(define (quicksort l)
  (if (or (null? l) (null? (cdr l))) l
      (append (quicksort (filter (lambda (x) (< x (car l))) l)) (filter (lambda (x) (= x (car l))) l) (quicksort (filter (lambda (x) (> x (car l))) l)))))

; Selection Sort
(define (selection-sort l)
  (if (or (null? l) (null? (cdr l))) l
      (let ((minimal (apply min l)))
        (cons minimal (selection-sort (remove-item l minimal))))))

(define (remove-item l number)
  (cond ((null? l) l)
        ((= number (car l)) (cdr l))
        (else (cons (car l) (remove-item (cdr l) number)))))

; Insertion Sort
(define (insertion-sort l)
  (define (insertion-sort-helper result l)
    (if (null? l) result
        (insertion-sort-helper (insert-sorted (car l) result) (cdr l))))
  (if (or (null? l) (null? (cdr l))) l
      (insertion-sort-helper '() l)))

(define (insert-sorted x l)
  (cond ((null? l) (cons x '()))
        ((< x (car l)) (cons x l))
        (else (cons (car l) (insert-sorted x (cdr l))))))

; Test Quicksort по-бърз от Merge-Sort
; (quicksort (generate-random2 1000000))
; (merge-sort (generate-random2 1000000))


; Test Selection-Sort по-бавен от Quicksort и Merge-Sort
; (quicksort (generate-random2 500000))
; (merge-sort (generate-random2 500000))
; (selection-sort (generate-random2 25000))

; Test Insertion-Sort по-бърз от Selection-Sort
; (insertion-sort (generate-random2 15000))
; (selection-sort (generate-random2 15000))

; Изводи:
; При Quicksort и Mergesort имаме почти еднакво време за сортиране, като Quicksort е малко по-бързо. Но например при 1,000,000 елемента, Quicksort се справя, докато Mergesort дава грешка, че не достига памет.
; Selection-Sort е по-бавен с 20,000 елемента, отколкото Quicksort с 1,000,000
; Insertion-Sort също е по-бавен Merge и Quicksort, но пък е по-бърз от Selection-Sort