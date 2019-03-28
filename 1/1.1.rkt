#lang racket

;Exercise1.1
;四道题需要先转换题目定义为递推公式
;1
(define (3n+2? n)
  (cond
    [(= n 2) #t]
    [(= n 5) #t]
    [else (let ([a (- n 3)])
            (cond
              [(>= a 5) (3n+2? a)]
              [else #f]))]))

(define (test-1 f l)
  (cond
    [(null? l) '()]
    [else (cons `(,(car l) ,(f (car l)))
                (test-1 f (cdr l)))]))

(test-1 3n+2? '(1 2 3 5 6 7 8 9 11 13 14 16 17))

;2
(define (1+a? a n)
  (cond
    [(= n 1) #t]
    [else (let ([x (- n a)])
            (cond
              [(>= x 1) (1+a? a x)]
              [else #f]))]))

(define (2n+3m+1? n)
  (or (1+a? 2 n) (1+a? 3 n)))

(test-1 2n+3m+1? '(0 1 2 3 4 5 6 7 8 9 10))

;3
(define (n-odd? l)
  (cond
    [(and (= (car l) 0)
          (= (cadr l) 1)) #t]
    [else (let ([a (- (car l) 1)]
                [b (- (cadr l) 2)])
            (cond
              [(and (>= a 0) (>= b 1))
               (n-odd? `(,a ,b))]
              [else #f]))]))

(test-1 n-odd? '((0 1) (0 2) (1 9) (3 7) (3 10) (4 9)))

;4
(define (n-n^2? l)
  (cond
    [(and (= (car l) 0)
          (= (cadr l) 0)) #t]
    [else (let* ([a (- (car l) 1)]
                 [b (- (cadr l) 1 (* a 2))])
            (cond
              [(and (>= a 0) (>= b 0))
               (n-n^2? `(,a ,b))]
              [else #f]))]))

(test-1 n-n^2? '((0 1) (1 1) (2 9) (3 9) (2 4)))

;Exercise1.2
;1
(define (n1k7? l)
  (cond
    [(and (= (car l) 0)
          (= (cadr l) 1)) #t]
    [else (let ([a (- (car l) 1)]
                [b (- (cadr l) 7)])
            (if (and (>= a 0) (>= b 1))
                (n1k7? `(,a ,b)) #f))]))

(test-1 n1k7? '((1 8) (2 15) (3 8)))

;2
(define (n2k? l)
  (cond
    [(and (= (car l) 0)
          (= (cadr l) 1)) #t]
    [else (let ([a (- (car l) 1)]
                [b (/ (cadr l) 2)]);b必须是偶数
            (if (and (>= a 0) (integer? b) (>= b 1))
                (n2k? `(,a ,b)) #f))]))

(test-1 n2k? '((1 2) (1 3) (2 4) (2 8) (3 8)))

;3
(define (n1ji+j? l)
  (cond
    [(and (= (car l) 0)
          (= (cadr l) 0)
          (= (caddr l) 1)) #t]
    [else (let* ([a (- (car l) 1)]
                 [c (cadr l)]
                 [b (- (caddr l) c)])
            (if (and (>= a 0) (>= b 0) (>= c 1))
                (n1ji+j? `(,a ,b ,c)) #f))]))

(test-1 n1ji+j? '((1 1 1) (2 2 3) (2 1 2)
                          (3 2 3) (4 3 5)
                          (5 5 1) (5 5 8)))

;4
(define (n1i2ij? l)
  (cond
    [(and (= (car l) 0)
          (= (cadr l) 1)
          (= (caddr l) 0)) #t]
    [else (let* ([a (- (car l) 1)]
                 [b (- (cadr l) 2)]
                 [c (- (caddr l) b)])
            (if (and (>= a 0) (>= b 1) (>= c 0))
                (n1i2ij? `(,a ,b ,c)) #f))]))

(test-1 n1i2ij? '((0 1 4) (1 3 1) (2 5 5)
                          (3 7 9) (5 11 22) (6 13 36)))

;总结：本节所有程序的特点是对参数求逆，然后递归下降直到平凡分支。

;Exercise1.4
;List-of-Int ::= () | (Int . List-of-Int)
(define (Lint-of-Int? l)
  (cond
    [(null? l) #t]
    [else (and (integer? (car l))
               (Lint-of-Int? (cdr l)))]))

(Lint-of-Int? '(-7 . (3 . (14 . ()))))

