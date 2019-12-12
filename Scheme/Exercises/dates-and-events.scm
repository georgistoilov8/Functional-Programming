#lang racket
(require rackunit rackunit/text-ui)

; Проверка дали число е месец.
(define (month? month)
  (and (> month 0) (< month 13)))

; Проверка дали ден е в даден интервал
(define (day-between? day start end)
  (and (>= day start) (<= day end)))

; Проверка дали година е високосна
(define (leap? year)
  (or (and (= (remainder year 4) 0) (not (= (remainder year 100) 0))) (= (remainder year 400) 0)))

; Връща наредена двойка от ден и наредена двойка от месец и година
; (day (month year))
(define (make-date day month year)
  (cons day (cons month year)))

; 1.) Day, month, year функции
(define day car)
(define month cadr)
(define year cddr)

; 2.) date?
(define (date? x)
  (and (pair? x) (pair? (cdr x)) (valid-date? x)))

(define (valid-date? date)
  (let ((day (day date))
        (month (month date))
        (year (year date)))
    (cond ((not (month? month)) #f)
          ((= month 2)
           (if (leap? year)
               (if (day-between? day 1 29) #t
                   #f)
               (if (day-between? day 1 28) #t
                   #f)))
          (else (case month
                  ((1 3 5 7 8 10 12) (if (day-between? day 1 31) #t
                                         #f))
                  (else (if (day-between? day 1 30) #t
                            #f)))))))

; 3.) date->string - По дадена дата връща низ "<ден>.<месец>.<година>"
(define (date->string date)
  (if (not (date? date)) "Not valid date (date->string)"
      (string-append (number->string (day date))
                     "."
                     (number->string (month date))
                     "."
                     (number->string (year date)))))

; 4.) next-day - По дадена дата връща датата на следващия ден.
(define (next-day date)
  (if (not (date? date)) "Not valid date (next-day)"
      (find-next (day date) (month date) (year date))))

(define (find-next day month year)
  (let ((option1 (make-date (+ 1 day) month year))
        (option2 (make-date 1 (+ 1 month) year))
        (option3 (make-date 1 1 (+ 1 year))))
    (cond ((date? option1) option1)
          ((date? option2) option2)
          (else option3))))

; 5.) date< - По две дати връща истина, ако първата е хронологически преди втората, лъжа в противен случай
(define (date< x y)
  (if (or (not (date? x)) (not (date? y)))"Not valid first date (date<)"
      (cond ((< (year x) (year y)) #t)
            ((and (= (year x) (year y)) (< (month x) (month y))) #t)
            ((and (= (year x) (year y)) (= (month x) (month y)) (< (day x) (day y))) #t)
            (else #f))))

(define (date= x y)
  (and (= (day x) (day y)) (= (month x) (month y)) (= (year x) (year y))))

; 6.) weekday - По дата връща кой ден от седмицата се пада
(define (weekday date)
  (if (not (date? date)) "Not valid date (weekday)"
      (case (modulo (+ (JDN date) 1) 7)
        ((0) `Sunday)
        ((1) `Monday)
        ((2) `Tuesday)
        ((3) `Wednesday)
        ((4) `Thursday)
        ((5) `Friday)
        ((6) `Saturday))))

(define (JDN date)
  (let ((D (day date))
        (M (month date))
        (Y (year date)))
    (+ (quotient (* 1461 (+ Y 4800 (quotient (- M 14) 12))) 4)
       (quotient (* 367 (+ M (- 2) (* (- 12) (quotient (- M 14) 12)))) 12)
       (- (quotient (* 3 (quotient (+ Y 4900 (quotient (- M 14) 12)) 100)) 4))
       D (- 32075))))

; 7.) next-weekday - приема ден от седмицата и дата d. Функцията връща най-ранната дата след d, падаща се на подадения ден от седмицата.
(define (next-weekday wday date)
  (if (not (date? date)) "Not valid date (next-weekday)"
      (if (equal? wday (weekday date)) date
          (next-weekday wday (next-day date)))))

;;; Събития
; 8.) events-for-day - по дата и списък от събития връща списък от всички събития в същия ден
(define (events-for-day date l)
  (cond ((not (date? date)) "Not valid date (events-for-day)")
        ((null? l) l)
        ((date= date (caar l)) (cons (cons date (cdr (car l))) (events-for-day date (cdr l))))
        (else (events-for-day date (cdr l)))))

; 9.) calendar
(define (del-assoc key value alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (add-assoc key value alist)
  (cons (cons key value) (del-assoc key value alist)))

(define (change-value key value alist)
  (let ((result (assoc key alist)))
    (if (equal? result #f) (list value)
        (append (cdr result) (list value)))))

(define (calendar l)
  (define (calendar-helper l alist)
    (if (null? l) alist
        (calendar-helper (cdr l) (add-assoc (caar l) (change-value (caar l) (cdr (car l)) alist) alist))))
  (sort (calendar-helper l '())))
      
; Sort of dates
(define (sort l)
  (if (or (null? l) (null? (cdr l))) l
      (append (sort (filter (lambda (x) (date< (car x) (car (car l)))) l)) (filter (lambda (x) (date= (car x) (car (car l)))) l) (sort (filter (lambda (x) (date< (car (car l)) (car x))) l)))))


;;; Unit-tests
(define make-date-tests
  (test-suite "Make date tests"
              (test-case "Should return (24 . (5 . 1914))" (check-equal? (make-date 24 5 1914) '(24 . (5 . 1914)) ))
              (test-case "Should return (10 . (12 . 2019))" (check-equal? (make-date 10 12 2019) '(10 . (12 . 2019)) ))
              ))
(run-tests make-date-tests 'verbose)


(define getters-tests
  (test-suite "Get day, month, year tests"
              (test-case "Should return 28 from day(28.02.2019)" (check-eq? (day (make-date 28 2 2019)) 28))
              (test-case "Should return 2 from month(28.02.2019)" (check-eq? (month (make-date 28 2 2019)) 2))
              (test-case "Should return 2019 from year(28.02.2019)" (check-eq? (year (make-date 28 2 2019)) 2019))
              (test-case "Should return 1 from day(01.01.2000)" (check-eq? (day (make-date 1 1 2000)) 1))
              (test-case "Should return 1 from month(01.01.2000)" (check-eq? (month (make-date 1 1 2000)) 1))
              (test-case "Should return 2000 from year(01.01.2000)" (check-eq? (year (make-date 1 1 2000)) 2000))
              ))
(run-tests getters-tests 'verbose)

(define is-date-tests
  (test-suite "Is date tests"
              (test-case "Should return true if date is 10.12.2019" (check-true (date? (make-date 10 12 2019))))
              (test-case "Should return true if date is 27.04.2013" (check-true (date? (make-date 27 4 2013))))
              (test-case "Should return true if date is 31.04.2019" (check-true (date? (make-date 31 8 2019))))
              (test-case "Should return true if date is 29.02.2000" (check-true (date? (make-date 29 2 2000))))
              (test-case "Should return true if date is 29.02.2020" (check-true (date? (make-date 29 2 2020))))
              (test-case "Should return false if date is 29.02.2019" (check-false (date? (make-date 29 2 2019))))
              (test-case "Should return false if date is 31.04.2019" (check-false (date? (make-date 31 4 2019))))
              (test-case "Should return false if date is 34.10.2019" (check-false (date? (make-date 34 10 2019))))
              (test-case "Should return false if date is 29.13.2019" (check-false (date? (make-date 29 13 2019))))
              (test-case "Should return false if date is 29.-2.2019" (check-false (date? (make-date 29 -2 2019))))
              (test-case "Should return true if date is 10.12.-2020" (check-true (date? (make-date 10 12 -2020))))
              ))
(run-tests is-date-tests 'verbose)

(define date-to-string-tests
  (test-suite "Date to string tests"
              (test-case "Should return 10.12.2019 for date 10.12.2019" (check-equal? (date->string (make-date 10 12 2019)) "10.12.2019"))
              (test-case "Should return 1.2.-1239 for date 01.02.-1239" (check-equal? (date->string (make-date 1 2 -1239)) "1.2.-1239"))
              ))
(run-tests date-to-string-tests 'verbose)

(define next-day-tests
  (test-suite "Next day tests"
              (test-case "Should return 11.12.2019 for date 10.12.2019" (check-equal? (date->string (next-day (make-date 10 12 2019))) "11.12.2019"))
              (test-case "Should return 27.4.2019 for date 26.04.2019" (check-equal? (date->string (next-day (make-date 26 4 2019))) "27.4.2019"))
              (test-case "Should return 1.4.2019 for date 31.03.2019" (check-equal? (date->string (next-day (make-date 31 3 2019))) "1.4.2019"))
              (test-case "Should return 1.5.2019 for date 30.04.2019" (check-equal? (date->string (next-day (make-date 30 4 2019))) "1.5.2019"))
              (test-case "Should return 1.3.2019 for date 28.02.2019" (check-equal? (date->string (next-day (make-date 28 2 2019))) "1.3.2019"))
              (test-case "Should return 29.2.2019 for date 28.02.2020" (check-equal? (date->string (next-day (make-date 28 2 2020))) "29.2.2020"))
              (test-case "Should return 1.1.2020 for date 31.12.2019" (check-equal? (date->string (next-day (make-date 31 12 2019))) "1.1.2020"))
              ))
(run-tests next-day-tests 'verbose)

(define date-less-tests
  (test-suite "Date less tests"
              (test-case "Should return true for date< 10.12.2019 11.12.2019" (check-true (date< (make-date 10 12 2019) (make-date 11 12 2019))))
              (test-case "Should return true for date< 10.12.2019 1.1.2020" (check-true (date< (make-date 10 12 2019) (make-date 1 1 2020))))
              (test-case "Should return true for date< 10.12.2019 11.12.2019" (check-true (date< (make-date 10 11 2019) (make-date 10 12 2019))))
              (test-case "Should return false for date< 11.12.2019 10.12.2019" (check-false (date< (make-date 11 12 2019) (make-date 10 12 2019))))
              (test-case "Should return false for date< 10.12.2020 1.1.2019" (check-false (date< (make-date 10 12 2020) (make-date 1 1 2019))))
              (test-case "Should return false for date< 10.12.2019 11.11.2019" (check-false (date< (make-date 10 12 2019) (make-date 10 11 2019))))
              ))
(run-tests date-less-tests 'verbose)

(define weekday-tests
  (test-suite "Weekday tests"
              (test-case "Should return Tuesday for 10.12.2019" (check-equal? (weekday (make-date 10 12 2019)) 'Tuesday ))
              (test-case "Should return Wednesday for 11.12.2019" (check-equal? (weekday (make-date 11 12 2019)) 'Wednesday ))
              (test-case "Should return Friday for 13.12.2019" (check-equal? (weekday (make-date 13 12 2019)) 'Friday ))
              (test-case "Should return Monday for 23.09.2019" (check-equal? (weekday (make-date 23 9 2019)) 'Monday ))
              (test-case "Should return Saturday for 22.2.2014" (check-equal? (weekday (make-date 22 2 2014)) 'Saturday ))
              (test-case "Should return Sunday for 31.05.2020" (check-equal? (weekday (make-date 31 5 2020)) 'Sunday ))
              (test-case "Should return Thursday for 12.07.2018" (check-equal? (weekday (make-date 12 7 2018)) 'Thursday ))
              ))
(run-tests weekday-tests 'verbose)

(define next-weekday-tests
  (test-suite "Date less tests"
              (test-case "Should return 13.12.2019 for 'Friday 10.12.2019" (check-equal? (date->string (next-weekday 'Friday (make-date 10 12 2019))) "13.12.2019" ))
              (test-case "Should return 4.1.2020 for 'Saturday 31.12.2019" (check-equal? (date->string (next-weekday 'Saturday (make-date 31 12 2019))) "4.1.2020" ))
              (test-case "Should return 29.2.2020 for 'Saturday 24.02.2020" (check-equal? (date->string (next-weekday 'Saturday (make-date 24 2 2020))) "29.2.2020" ))
              (test-case "Should return 10.12.2019 for 'Tuesday 10.12.2019" (check-equal? (date->string (next-weekday 'Tuesday (make-date 10 12 2019))) "10.12.2019" )) 
              ))
(run-tests next-weekday-tests 'verbose)

(define events (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                      (cons (make-date 27 11 2019) "Спират водата в Младост")
                      (cons (make-date 28 11 2019) "Спират водата в Лозенец")
                      (cons (make-date 29 10 2018) "Няма ток в Слатина")
                      (cons (make-date 29 3 2020) "България се класира на Европейското първенство по футбол")
                      (cons (make-date 18 12 2019) "Барселона срещу Реал Мадрид")
                      (cons (make-date 15 11 2014) "Кличко побеждава Пулев")
                      (cons (make-date 29 10 2018) "Излиза нова песен на Ицо Хазарта")))

(define events-for-day-tests
  (test-suite "Events for day tests"
              (test-case "Should return '(((27 11 . 2019) . Първа лекция за Хаскел) ((27 11 . 2019) . Спират водата в Младост)) for events at date 27.11.2019" (check-equal? (events-for-day (make-date 27 11 2019) events) '(((27 11 . 2019) . "Първа лекция за Хаскел") ((27 11 . 2019) . "Спират водата в Младост"))))
              (test-case "Should return '(((29 3 . 2020) . България се класира на Европейското първенство по футбол)) for events at date 29.3.2020" (check-equal? (events-for-day (make-date 29 3 2020) events) '(((29 3 . 2020) . "България се класира на Европейското първенство по футбол"))))
              (test-case "Should return '(((29 10 . 2018) . Няма ток в Слатина) ((29 10 . 2018) . Излиза нова песен на Ицо Хазарта) ) for events at date 29.10.2018" (check-equal? (events-for-day (make-date 29 10 2018) events) '(((29 10 . 2018) . "Няма ток в Слатина") ((29 10 . 2018) . "Излиза нова песен на Ицо Хазарта")) ))
              ))
(run-tests events-for-day-tests 'verbose)

(define calendar-tests
  (test-suite "Calendar tests"
              (test-case "Should return '(((15 11 . 2014) . Кличко побеждава Пулев) ((29 19 . 2018) . Няма ток в Слатина Излиза нова песен на Ицо Хазарта) ((27 11 . 2019) . Първа лекция за Хаскел  Спират водата в Младост)
 ((28 11 . 2019) . Спират водата в Лозенец) ((18 12 . 2019) . Барселона срещу Реал Мадрид) ((29 3 . 2020) . България се класира на Европейското първенство по футбол))  for events at date 27.11.2019"
                         (check-equal? (calendar events)
                                       '(((15 11 . 2014) "Кличко побеждава Пулев") ((29 10 . 2018) "Няма ток в Слатина" "Излиза нова песен на Ицо Хазарта") ((27 11 . 2019) "Първа лекция за Хаскел" "Спират водата в Младост")
 ((28 11 . 2019) "Спират водата в Лозенец") ((18 12 . 2019) "Барселона срещу Реал Мадрид") ((29 3 . 2020) "България се класира на Европейското първенство по футбол")))
              )))
(run-tests calendar-tests 'verbose)