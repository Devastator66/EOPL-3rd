#lang racket

;Exercise1.7
(define (nth-element lst n)
  (cond
    [(< (length lst) (add1 n))
     (error 'nth-element
            "~s does not have ~s elements. ~%" lst n)]
    [else (if (zero? n)
              (car lst)
              (nth-element (cdr lst) (sub1 n)))]))

(nth-element '(a b c d e) 4)
;(nth-element '(a b c) 8)

;Exercise1.9
(define (remove s los)
  (cond
    [(null? los) '()]
    [else (if (eqv? (car los) s)
              (remove s (cdr los))
              (cons (car los)
                    (remove s (cdr los))))]))

(remove 'a '(a b c d a e f a g))


;Exercise1.12
(define (subst new old slist)
  (cond
    [(null? slist) '()]
    [(symbol? (car slist))
     (if (eqv? (car slist) old)
         (cons new (subst new old (cdr slist)))
         (cons (car slist) (subst new old (cdr slist))))]
    [(list? (car slist))
     (cons (subst new old (car slist))
           (subst new old (cdr slist)))]))

(subst 'a 'x '((x b c) x (d x) e f x w))

;Exercise1.13
(define (subst-kl new old slist)
  (letrec ([subst-in-exp
            (lambda (e)
              (cond
                [(symbol? e)
                 (if (eqv? e old)
                     new e)]
                [else (subst-kl new old e)]))])
    (map subst-in-exp slist)))

(subst-kl 'a 'x '((x b c) x (d x) e f x w))
           

