#lang racket

;Exercise1.15
(define (duple n x)
  (cond
    [(= n 0) '()]
    [else (cons x (duple (sub1 n) x))]))

(duple 2 3)
(duple 4 '(ha ha))

;Exercise1.16
(define (invert lst)
  (cond
    [(null? lst) '()]
    [else
     (let ([a (car (car lst))]
           [b (cadr (car lst))])
       (cons `(,b ,a)
             (invert (cdr lst))))]))

(invert '((a 1) (a 2) (1 b) (2 b)))

;Exercise1.17
(define (down lst)
  (cond
    [(null? lst) '()]
    [else (cons `(,(car lst))
                (down (cdr lst)))]))

(down '(1 2 3))

;Exercise1.18
(define (swapper s1 s2 slist)
  (cond
    [(null? slist) '()]
    [(symbol? (car slist))
     (cond
       [(eqv? (car slist) s1)
        (cons s2 (swapper s1 s2 (cdr slist)))]
       [(eqv? (car slist) s2)
        (cons s1 (swapper s1 s2 (cdr slist)))]
       [else
        (cons (car slist)
              (swapper s1 s2 (cdr slist)))])]
    [(list? (car slist))
     (cons (swapper s1 s2 (car slist))
           (swapper s1 s2 (cdr slist)))]))

(swapper 'a 'd '(a b c d))

;Exercise1.19
(define (list-set lst n x)
  (cond
    [(null? lst) '()]
    [(= n 0) (cons x (cdr lst))]
    [else (cons (car lst)
                (list-set (cdr lst)
                          (sub1 n)
                          x))]))

(list-set '(a b c d) 2 '(1 2))

;Exercise1.20
(define (count-occurrences s slist)
  (cond
    [(null? slist) 0]
    [(symbol? (car slist))
     (if (eqv? s (car slist))
         (add1 (count-occurrences s (cdr slist)))
         (count-occurrences s (cdr slist)))]
    [(list? (car slist))
     (+ (count-occurrences s (car slist))
        (count-occurrences s (cdr slist)))]))

(count-occurrences 'x '((f x) y (((x z) x))))

;Exercise1.21
(define (product sos1 sos2)
  (cond
    [(or (null? sos1)
         (null? sos2)) '()]
    [else (append (put1 (car sos1) sos2)
                  (product (cdr sos1) sos2))]))

(define (put1 a l)
  (cond
    [(null? l) '()]
    [else (cons `(,a ,(car l))
                (put1 a (cdr l)))]))

(product '(a b c) '(x y))

;Exercise1.22
(define (filter-in pred lst)
  (cond
    [(null? lst) '()]
    [else (if (pred (car lst))
              (cons (car lst)
                    (filter-in pred (cdr lst)))
              (filter-in pred (cdr lst)))]))

(filter-in number? '(a 2 (1 3) b 7))

;Exercise1.23
(define (list-index pred lst)
  (letrec
      ([n-th (lambda (lst n)
               (cond
                 [(null? lst) #f]
                 [(pred (car lst)) n]
                 [else (n-th (cdr lst)
                             (add1 n))]))])
    (n-th lst 0)))

(list-index symbol? '(a (b c) 17 foo))
(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(1 2 (a b) 3))

;Exercise1.24
(define (every? pred lst)
  (cond
    [(null? lst) #t]
    [else (and (pred (car lst))
               (every? pred (cdr lst)))]))

(every? number? '(a b c 3 e))
(every? number? '(1 2 3 4 5))

;Exercise1.25
(define (exists? pred lst)
  (cond
    [(null? lst) #f]
    [else (or (pred (car lst))
              (exists? pred (cdr lst)))]))

(exists? number? '(a b c 3 e))
(exists? number? '(a b c d e))

;Exercise1.26
(define (up lst)
  (cond
    [(null? lst) '()]
    [(list? (car lst))
     (append (car lst)
             (up (cdr lst)))]
    [else (cons (car lst)
                (up (cdr lst)))]))

(up '((1 2) (3 4)))
(up '((x (y)) z))

;Exercise1.27
(define (flatten slist)
  (cond
    [(null? slist) '()]
    [(list? (car slist))
     (append (flatten (car slist))
             (flatten (cdr slist)))]
    [else (cons (car slist)
                (flatten (cdr slist)))]))

(flatten '(a b c))
(flatten '((a) () (b ()) () (c)))
(flatten '((a b) c (((d)) e)))

;Exercise1.28
(define (merge loi1 loi2)
  (cond
    [(null? loi1) loi2]
    [else (merge (cdr loi1)
                 (insert (car loi1)
                         loi2))]))

(define (insert a l)
  (cond
    [(null? l) `(,a)]
    [else (if (<= a (car l))
              (cons a l)
              (cons (car l)
                    (insert a (cdr l))))]))

(merge '(1 4) '(1 2 8))

;Exercise1.29
(define (sort loi)
  (cond
    [(null? loi) '()]
    [else (insert (car loi)
                  (sort (cdr loi)))]))

(sort '(8 2 5 2 3))

;Exercise1.30
(define (ins pred a l)
  (cond
    [(null? l) `(,a)]
    [else (if (pred a (car l))
              (cons a l)
              (cons (car l)
                    (ins pred a (cdr l))))]))

(define (sort/predicate pred loi)
  (cond
    [(null? loi) '()]
    [else (ins pred (car loi)
               (sort/predicate pred (cdr loi)))]))

(sort/predicate < '(8 2 5 2 3))
(sort/predicate > '(8 2 5 2 3))

;Exercise1.31
(define (leaf n)
  n)

(define (interior-node sym left right)
  `(,sym ,left ,right))

(define (leaf? Btree)
  (integer? Btree))

(define (lson interior)
  (cadr interior))

(define (rson interior)
  (caddr interior))

(define (contents-of Btree)
  (if (integer? Btree)
      Btree
      (car Btree)))

;Exercise1.32
(define (double-tree bintree)
  (cond
    [(leaf? bintree) (* 2 bintree)]
    [else `(,(contents-of bintree)
            ,(double-tree (lson bintree))
            ,(double-tree (rson bintree)))]))

(define t (double-tree '(a (b 3 2) (c 6 9))))
(cadr (cadr t))
  
;Exercise1.33
(define (mlwrd Btree n)
  (cond
    [(leaf? Btree) n]
    [else
     (let ([s (contents-of Btree)]
           [l (lson Btree)]
           [r (rson Btree)])
       (cond
         [(and (leaf? l) (leaf? r))
          `(,s ,n ,n)]
         [(leaf? l) `(,s ,n ,(mlwrd r (add1 n)))]
         [(leaf? r) `(,s ,(mlwrd l (add1 n)) ,n)]
         [else `(,s ,(mlwrd l (add1 n))
                    ,(mlwrd r (add1 n)))]))]))

(define (mark-leaves-with-red-depth bintree)
  (mlwrd bintree 0))

(define red
  (interior-node 'red
                 (interior-node 'bar
                                (leaf 26)
                                (leaf 12))
                 (interior-node 'red
                                (leaf 11)
                                (interior-node 'quux
                                               (leaf 117)
                                               (leaf 14)))))

(mark-leaves-with-red-depth red);此题书中有误

;Exercise1.34
(define (path n bst)
  (cond
    [(null? bst) '()]
    [(= n (car bst)) '()]
    [else (if (< n (car bst))
              (cons 'left (path n (cadr bst)))
              (cons 'right (path n (caddr bst))))]))

(path 17 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))

;Exercise1.35
(define (num-tree n Btree)
  (cond
    [(leaf? Btree) n]
    [else
     (let ([s (contents-of Btree)]
           [l (lson Btree)]
           [r (rson Btree)])
       (cond
         [(and (leaf? l) (leaf? r))
          `(,s ,n ,(add1 n))]
         [(leaf? l)
          `(,s ,n ,(num-tree (add1 n) r))]
         [(leaf? r)
          (let ([lt (num-tree n l)])
            `(,s ,lt ,(add1 (lastn lt))))]
         [else
          (let* ([lt (num-tree n l)]
                 [next (add1 (lastn lt))])
            `(,s ,lt ,(num-tree next r)))]))]))

;Bintree -> num
;得到极右叶子的编号
(define (lastn Btree)
  (cond
    [(leaf? Btree) (contents-of Btree)]
    [else (if (leaf? (rson Btree))
              (contents-of (rson Btree))
              (lastn (rson Btree)))]))

(define (number-leaves btree)
  (num-tree 0 btree))

(number-leaves
 (interior-node 'foo
                (interior-node 'bar
                               (leaf 26)
                               (leaf 12))
                (interior-node 'baz
                               (leaf 11)
                               (interior-node 'quux
                                              (leaf 117)
                                              (leaf 14)))))

;Exercise1.36
;不知道这两个算不算答案
;(define (g e-e l2e)
;  (let ([L (length l2e)])
;    (cons (list L
;                (cadr e-e))
;          l2e)))

(define (g e-e l2e)
  (cond
    [(null? l2e) (cons e-e l2e)]
    [else (cons `(,(sub1 (car (car l2e)))
                  ,(cadr e-e))
                l2e)]))

(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst))
           (number-elements (cdr lst))))))

(number-elements '(a b c d e f))