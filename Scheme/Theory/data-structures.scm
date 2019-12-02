#lang racket

;;;  Структури от данни в Scheme

;;; Матрици

;;; Представяне на матрица:
;;; ( (1 2 3) (4 5 6) ) е ( 1 2 3 )
;;;                       ( 3 4 5 )

;;; Проверка за коректност на матрица
(define (all? p? l)
  (foldr (lambda (x y) (and x y)) #t (map p? l)))

(define (matrix? m)
  (and (list? m)
       (not (null? (car m)))))

;;; Базови операции с матрици

;;; Брой редове
(define (get-rows m) (length m))
;(get-rows '( (1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;;; Брой стълбове
(define (get-columns m)
  (if (null? m) 0
      (length (car m))))
;(get-columns '( (1 2) (3 4) (5 6) (7 8)))

;;; Намиране на първи ред
(define (first-row m)
  (if (null? m) m
      (car m)))
;(first-row '( (1 2) (3 4) (5 6) (7 8)))
;;; Съкратен вариант
(define get-first-row car)


;;; Намиране на първа колона
(define (first-column m)
  (if (null? m) m
      (if (null? (car m)) '()
                 (cons (car (car m)) (first-column (cdr m))))))
;(first-column '( (1 2) (3 4) (5 6) (7 8)))
;;; Съкратен вариант(за досетливите)
(define (get-first-column m) (map car m))

;;; Изтриване на първи ред
(define remove-first-row cdr)
;(remove-first-row '( (1 2) (3 4) (5 6) (7 8)))

;;; Изтриване на първа колона
(define (remove-first-column m) (map cdr m))
;(remove-first-column '( (1 2) (3 4) (5 6) (7 8)))

;;; Намиране на ред по индекс
(define (get-row index m) (list-ref m index))
;(get-row 2 '( (1 2) (3 4) (5 6) (7 8)))

;;; Намиране на стълб по индекс
(define (get-column index m)
  (map (lambda (row) (list-ref row index)) m))
;(get-column 1 '( (1 2) (3 4) (5 6) (7 8)))

;;; Транспониране на матрица
;;; Вариант 1(директна рекурсия
(define (transpose m)
  (if (null? (get-first-row m)) '()
      (cons (first-column m) (transpose (remove-first-column m)))))
;(transpose '( (1 2) (3 4) (5 6) (7 8)))

;;; Вариант 2(accumulate)
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (transpose2 m)
  (accumulate cons '() 0 (- (get-columns m) 1) (lambda (index) (get-column index m)) (lambda (x) (+ x 1))))
;(transpose2 '( (1 2) (3 4) (5 6) (7 8)))

;;; Аритметични операции над матрици
; Събиране на матрици
(define (sum-vectors v1 v2) (map + v1 v2))
(define (sum-matrices m1 m2) (map sum-vectors m1 m2))
;(sum-matrices '((1 2) (3 4)) '((5 6) (7 8)))

; Умножение на матрици
(define (mult-vectors v1 v2) (apply + (map * v1 v2)))
(define (mult-matrices m1 m2)
  (let ((m2t (transpose m2)))
    (map (lambda (row)
           (map (lambda (column) (mult-vectors row column)) m2t))
         m1)))
;(mult-matrices '( (5 8 -4) (6 9 -5) (4 7 -2)) '( (2) (-3) (1)))

;;; Двоични дървета
; Представяне: (<корен> <ляво> <дясно>)
; Пример:(1 (2 () ())
;           (3 (4 () ())
;              (5 () ())))

; Проверка за коренктност
(define (tree? t)
  (or (null? t)
      (and (list t) (= (length t) 3))
      (tree? (cadr t))
      (tree? (caddr t))))

; Конструктори:
(define empty-tree '())
(define (make-tree root left right) (list root left right))

; Селектори
; Взимане на корена на дървото
(define (get-root t) (car t))
; Взимане на лявото дете на корена
(define (get-left t) (car (cdr t)))
; Взимане на дясното дете на корена
(define (get-right t) (car (cdr (cdr t))))
; Проверка дали дървото е празно
(define (empty-tree? t) (if (null? t) #t #f))

; Дълбочина на дърво
(define (tree-depth t)
  (if (empty-tree? t) 0
      (max (+ 1 (tree-depth (get-left t)))
           (+ 1 (tree-depth (get-right t))))))
; (tree-depth '(1 (2 () ()) (3 (4 () ()) (5 () ()))))

; Намиране на поддърво
(define (memv-tree x t)
  (cond ((empty-tree? t) #f)
        ((eqv? x (get-root t)) t)
        (else (or (memv-tree x (get-left t))
                  (memv-tree x (get-right t))))))
;(memv-tree 3 '(1 (2 () ()) (3 (4 () ()) (5 () ()))))

; Търсене на път в двоично дърво
(define (path-tree x t)
  (cond ((empty-tree? t) #f)
        ((eqv? x (get-root t)) (list x))
        (else (cons#f (get-root t)
                      (or (path-tree x (get-left t))
                          (path-tree x (get-right t)))))))
(define (cons#f h t) (and t (cons h t)))
;(path-tree 3 '(1 (2 () ()) (3 (4 () ()) (5 () ()))))

;;; Асоциативни списъци (речник, хеш, map)
; Пример: ((1 . 2) (2 . 3) (3 . 4))
;         ((a . 10) (b . 12) (c . 18))
;         ((l1 1 8) (l2 10 1 2) (l3))
;         ((al1 (1 . 2) (2 . 3)) (al2 (b)) (al3 (a . b) (c . d)))

; Създаване на асоциативен списък по списък от ключове и функция
(define (make-alist f keys)
  (map (lambda (x) (cons x (f x))) keys))
;(make-alist (lambda (x) (* x x)) '(1 3 5))


; Селектори за асоциативни списъци

; Връща ключовете на асоциативните списъци
(define (keys alist) (map car alist))
; Връща стойностите на асоциативните списъци
(define (values alist) (map cdr alist))

#| Не е тествано(има вграден assoc)
(define (assoc key alist)
  (if (null? alist) #f
      (if (equal? key (car (car alist))) (car alist)
          (assoc key (cdr alist)))))
|#
; (assv <ключ> <асоциативен-списък>) - същото като assoc, но сравнява с eqv?
; (assq <ключ> <асоциатевен-списък>) - същото като assoc, но сравнява с eq?

; Изтриване на ключ и съответната му стойност (ако съществува)
(define (del-assoc key alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))
; (del-assoc 3 '( (1 . 3) (2 . 4) (3 . 5)))

; Задаване на стойност за ключ (изтривайки старата ако има такава)
(define (add-assoc key value alist)
  (cons (cons key value) (del-assoc key alist)))

; Ако обаче искаме да запазим реда на ключовете? Имаме два варианта:
; Вариант 1(грозен и по-бърз)
(define (add-key-value key value alist)
  (let ((new-kv (cons key value)))
    (cond ((null? alist) (list new-kv))
          ((eqv? (caar alist) key) (cons new-kv (cdr alist)))
          (else (cons (car alist) (add-key-value key value (cdr alist)))))))

; Вариант 2(красив и по-бавен)
(define (add-key-value2 key value alist)
  (let ((new-kv (cons key value)))
    (if (assq key alist)
        (map (lambda (kv) (if (eq? (car kv) key)
                              new-kv kv)) alist)
        (cons new-kv alist))))

; Помощна функция(част от задача)
; Да се намери има ли елемент на l, който удовлетворява p.
(define (search p l)
  (and (not (null? l))
       (or (p (car l)) (search p (cdr l)))))
; Важно свойство: Ако p връща "свидетел" на истинността на свойството p (както
; например memv или assv), то search също връща този "свидетел".
; Пример:
(define (assq key al)
  (search (lambda (kv) (and (eq? (car kv) key) kv)) al))


;;; Графи
; Графите се представят чрез асоциативни списъци
; Ключът е върхът/възелът, а стойностите ще са възли,
; таки че има ребро между двата.
; Пример:((1 . (2 3))
;         (2 . (3 6))
;         (3 . (4 6))
;         (4 . (1 5))
;         (5 . (3))
;         (6 . (5)))

; Върховете на граф
(define (vertices g) (keys g))

; Децата на даден връх
(define (children v g)
  (cdr (assv v g)))

; Съществува ли ребро между два върха
(define (edge? u v g)
  (memv v (children u g)))

(define (map-children v f g)
  (map f (children v g)))

(define (search-child v f g)
  (search f (children v g)))

; Абстракция за граф чрез капсулация:
(define (make-graph g)
  (define (self prop . params)
    (case prop
      ('print g)
      ('vertices (keys g))
      ('children (let ((v (car params)))
                   (cdr (assv v g))))
      ('edge? (let ((u (car params)) (v (cadr params)))
               (memv v (self 'children u))))
      ('map-children (let ((v (car params))
                           (f (cadr params)))
                       (map f (self 'children v))))
      ('search-child (let ((v (car params))
                           (f (cadr params)))
                       (search f (self 'children v))))))
  self)

; Задачи:
; 1) Да се намерят върховете, които нямат деца.
(define (find-childless g)
  (filter (lambda (v) (null? (children v g))) (vertices g)))

; 2) Да се намерят родителите на даден връх.
(define (parents g vert)
  (filter (lambda (v) (edge? v vert g)) (vertices g)))

; 3) Да се провери дали граф е симетричен.
; Ако u -> v => v -> u за всяко u,v
(define (symmetric? g)
  (all? (lambda (u)
          (all? (lambda (v) (edge? v u g))
                (children u g)))
        (vertices g)))

; Обхождане на граф
; Обхождане в дълбочина
;(define (dfs u g)
;  (map (lambda (v) (? (dfs v g)))
;     (children u g)))

; Търсене на път в дълбочина
(define (dfs-path u v g)
  (define (dfs-search path)
    (let ((current (car path)))
      (cond ((eqv? current v) (reverse path))
            ((memv current (cdr path)) #f)
            (else (search-child current
                                (lambda (w) (dfs-search (cons w path))) g)))))
    (dfs-search (list u)))

; Обхождане в ширина
#|(define (bfs u g)
  (define (bfs-level l)
    (if (null? l) <дъно>
        (bfs-level
         (<функция-за-обработка> (lambda (v) (children v g))
                                 l))))
  (bfs-level (list u)))
|#

#|(define (extend path)
  (map-children (car path)
                (lambda (w) (cons w path)) g))

(define (remains-acyclic? path)
  (not (memv (car path) (cdr path))))

(define (extend-acyclic path)
  (filter remains-acyclic? (extend path)))
|#

; TODO: Търсене на най-кратък път от u до v, ако има такъв