;;; Списъци (list)

;;; Конструиране на наредена двойка:
;;; (cons <израз1> <израз2>
;;; Наредена двойка от оценките на <израз1> и <израз2>
;;; (car <израз>) - първият компонент на двойката, която е оценена на <израз>
;;; (cdr <израз>) - вторият компонент на двойката, която е оценена на <израз>
;;; (pair? <израз>) - проверява дали оценката на <израз> е наредена двойка

;;; Примери:
(cons (cons 2 3) (cons 8 13))
(cons 3 (cons (cons 13 21) 8))

;;; S-изрази:
;;; Дефиниция:
;;;   - атоми(булеви, числа, знаци, символи, низове, функции
;;;   - наредени двойки (S1 . S2), където S1 и S2 са S=изрази


;;; Списъци в Scheme
;;; Дефиниция:
;;;    1) Празният списък () е списък
;;;    2) (h . t) е списък ако t e списък
;;;        h - глава на списъка, t - опашка на списъка

;;; Вградени функции за списъци
;;; (null? <израз>) - дали <израз> е празния списък
(null? '(1 2 3))   ;;; -> #f
(null? '())        ;;; -> #t
;;; (list? <израз>) - дали <израз> е списък
;;; Празният списък е списък
(list? '(1 2 3))   ;;; -> #t
(list? '())        ;;; -> #t
(list? 2)          ;;; -> #f

;;; Дефиниране на функцията list?
(define (list2 l)
  (or (null? l) (and (pair? l) (list2 (cdr l)))))

;;; Построяване на списък
;;; (list <израз1> <израз2> ... <изразN>) <=> (cons <израз1> (cons <израз2> ... (cons <изразN> '()))...)
(list '(1 2 3) '(4 5 6))
(list 1 2 3 4 5 6)


;;; Глава на списък
;;; (car <списък>) - глава на <списък>
(car (list '(1 2 3) '(4 5 6)))
(car (list 1 2 3 4 5 6))

;;; Опашка на списък
;;; (cdr <списък>) - опашка на <списък>
(cdr (list '(1 2 3) '(4 5 6)))
(cdr (list 1 2 3 4 5 6))

;;; !!! () не е наредена двойка => не може да се извиква car и cdr над празния списък

;;; (car '()) => Грешка!, (cdr '()) => Грешка!

;;; Форми на равенство в Scheme
;;; 1.)(eq? <израз1> <израз2>) -> връща #t <=> оценките на <израз1> и <израз2> заемат едно и също място в паметта
;;; 2.)(eqv? <израз1> <израз2>) -> връща #t <=> оценките на <израз1> и <израз2> заемат едно и също място в паметта или са едни и същи по стойност атоми(дори и да заемат различно място в паметта)
;;; Ако eq?, то със сигурност и eqv?.
;;; 3.)(equal? <израз1> <израз2>) -> връща #t <=> оценките на <израз1> и <израз2> са едни и същи по стойност атоми или наредени двойки, чиито компоненти са равни в смисъла на equal?
;;; Ако eqv?, то със сигурност equal?
;;; equal? проверява за равенство на списъци

;;; Дължина на списък:
;;; (length <списък>)
(length '())
(length '(1 2 3 4 5 6 7 8 9))

;;; Конкатениране на списък
;;; (append {<списък>}
(append '(1 2 3) '(4 5 6))
(append '(1 2) '())

;;; Обръщане на елементите
;;; (reverse <списък>)
(reverse '(1 2 3 4 5))
(reverse (append (reverse '(1 2 3)) '(4 5 6)))

;;; Елементите на списък без първите N
;;; (list-tail <списък> n)
(list-tail '(1 2 3 4) 2)

;;; N-ти елемент на списък
;;; (list-ref <списък> n) Започва се от 0
(list-ref '(1 2 3 4) 0)
(list-ref (reverse (append '(1 2 3) '(4 5 6))) 3)

;;; Проверка дали елемент се среща в списък
;;; (member <елемент> <списък>)
(member 1 '(1 2 3))
;;; Връща елементите нататък ако го има.
;;; Ако не се среща връща #f
;;; Използва equal?
(member 2 '(1 2 3))

;;; (memv <елемент> <списък>) - като member, но сравнява с eqv?

;;; (memq <елемент> <списък>) - като member, но сравнява с eq?

;;; Обхождане на списъци
;;; При обхождане на l:
;;;    Ако l е празен, връщаме базова стойност(дъно)
;;;    Иначе, комбинираме главата (car l) с резултата от рекурсивното извикване над опашката (cdr l) (стъпки)
;;; Примери: length, list-tail, list-ref, member, memq, memv

;;; Конструиране на списъци
;;; Използваме рекурсия по даден параметър (напр. число, списък, ...)
;;;    На дъното връщаме фиксиран списък (например ())
;;;    На стъпката построяваме с cons списък със съответната глава, а опашката строим чрез рекурсивно извикване на същата функция.
;;; Примери: from-to, collect, append, reverse


;;; Функцията (map <функция> <списък>) (прилага <функция> над всеки елемент от <списък>)
(define (map2 f l)
  (if (null? l) l
      (cons (f (car l)) (map f (cdr l)))))
(define (square x) (* x x))
(define (1+ x) (+ x 1))
(map2 (lambda (x) (* x x)) '(1 2 3))
(map2 square '(1 2 3))
(map2 cadr '((a b c) (d e f) (g h i)))
(map2 (lambda (f) (f 2)) (list square 1+ odd?))


;;; Функция (filter <условие> <списък>) -> Връща само тези елементи, които отговарят на условието
(define (filter condition l)
  (if (null? l) l
      (if (condition (car l))
          (cons (car l) (filter condition (cdr l)))
          (filter condition (cdr l)))))
;;; Алтернативна дефиниция с cond
(define (filter2 p? l)
  (cond ( (null? l) l)
        ( (p? (car l)) (cons (car l) (filter2 p? (cdr l))))
        (  else (filter2 p? (cdr l)))))
(filter odd? '(1 2 3 4 5))
(filter2 odd? '(1 2 3 4 5))
(filter pair? '((a b) c () d (e)))
(map2 (lambda (x) (filter even? x)) '((1 2 3) (4 5 6) (7 8
9)))
(map (lambda (x) (map (lambda (f) (filter f x)) (list
negative? zero? positive?))) '((-2 1 0) (1 4 -1) (0 0
1)))


;;; Дясно свиване(foldr)
(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))
(foldr * 1 '(1 2 3 4 5))
(foldr + 0 (map square (filter odd? '(1 2 3 4 5))))
(foldr cons '() '(1 5 10))
(foldr list '() '(1 5 10))
(foldr append '() '((a b) (c d) (e f)))
;;; map, filter и accumulate може да се реализират чрез foldr

;;; Ляво свиване(foldl)
(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(foldl * 1 '(1 2 3 4 5))
(foldl cons '() '(1 5 10))
(foldl (lambda (x y) (cons y x)) '() '(1 5 10))
(foldl list '() '(1 5 10))
(foldl append '() '((a b) (c d) (e f)))

;;; foldr генерира линеен рекурсивен процес, а foldl - линеен итеративен

;;; Ако искаме да пропуснем нулевата стойност:
(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l)
          (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))
;;; Задачи

;;; Да се намери максималният елемент на списък
(define (max_element l)
  (foldr max (car l) l))

(max_element '(1 2 3 4))


;;; Дълбоки списъци
;;; Пример: ((1 (2)) (((3) 4) (5 (6)) () (7)) 8)
;;; Задача: Да се преброят атомите в дълбокия списък

(define (atom? x) (and (not (null? x)) (not (pair? x))))

(define (count-atoms l)
  ( cond ( (null? l) 0)
         ( (atom? l) 1)
         ( else (+ (count-atoms (car l)) (count-atoms (cdr l))))))

;;; Да се съберат всички атоми от дълбок списък
(define (flatten l)
  (cond ( (null? l) l)
        ( (atom? l) (list l))
        ( else (append (flatten (car l)) (flatten (cdr l))))))

(flatten '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

;;; Да се обърне редът на атомите в дълбок списък
(define (deep-reverse l)
  (cond ((null? l) l)
        ((atom? l) l)
        ( else (append (deep-reverse (cdr l))
                       (list (deep-reverse (car l)))))))

(deep-reverse '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

;;; Свиване на дълбоки списъци
;;; (deep-foldr <x-дъно> <в-дъно> <операция> <списък>)
(define (deep-foldr nv term op l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        ( else (op (deep-foldr nv term op (car l))
                   (deep-foldr nv term op (cdr l))))))

(define (count-atoms l) (deep-foldr 0 (lambda (x) 1) + l))
(count-atoms '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

;;; Вместо lambda може и само list
(define (flatten l) (deep-foldr '() (lambda (x) (list x)) append l))
(flatten '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

(define (snoc x l) (append l (list x)))
(define (deep-reverse l) (deep-foldr '() (lambda (x) x) snoc l))

;;; Как работи deep-foldr?
;;;    - пуска себе си рекурсивно за всеки елемент на дълбокия списък
;;;    - при достигане на вертикално дъно (атоми) се прилага терм върху тях
;;;    - и събира резултатите с op

;;; deep-foldr чрез map и foldr
(define (branch p? f g) (lambda (x) (p? x) (f x) (g x)))
(define (deep-foldr nv term op l)
  (foldr op nv
         (map (branch atom?
                      term
                      (lambda (l) (deep-foldr nv term op l))
                      l))))
