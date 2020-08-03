#lang s-exp "language.rkt"
;; Sum the odd numbers in a list
;;
;; Example: (sum-odd-numbers '(1 2 3 4 112 5))
;;    ==>   9
;; Example: (sum-odd-numbers '(2 4 6 8))
;;    ==>   0
;; Example: (sum-odd-numbers '())
;;    ==>   0
(define (sum-odd-numbers lst)
  (let ([sum 0])
    (if (null? lst)
        (+ sum 0)
        (if (null? (cdr lst))
            (if (odd?(car lst))
                (+ sum (car lst))
                (+ sum 0))
            (if (odd? (car lst))
                (+ sum (+ (car lst) (+ sum (sum-odd-numbers (cdr lst)))))
                (+ sum (sum-odd-numbers (cdr lst)))
            )
        )    
    )
  )
)

;; Product of the even numbers in the list. You should use define/match
;; for your solution.
;;
;; Example: (product-even-numbers '(2 1 6 3 5))
;;    ==>   12
;; Example: (product-even-numbers '(1 3 5 7))
;;    ==>   1
;; Example: (product-even-numbers '())
;;    ==>   1
(define (product-even-numbers lst)
  (define/match (recurse lst accumulator)
    [(_ _) (if (null? lst)
               (* accumulator 1)
               (if (null? (cdr lst))
                   (if (even? (car lst))
                       (* accumulator (car lst))
                       (* accumulator 1)
                   )
                   (if (even? (car lst))
                       (* accumulator (* (car lst) (recurse (cdr lst) accumulator)))
                       (* accumulator (recurse (cdr lst) accumulator)))
               )
           )
    ])
  (recurse lst 1))

;; Range takes two integers a and b, and returns a list from a
;; up till (but not including) b.
;;
;; Example: (range 1 10)
;;    ==>   '(1 2 3 4 5 6 7 8 9)
;;
;; If a is greater than or equal to b, just return the empty
;; list:
;;
;; Example: (range 10 10)
;;    ==>   '()
;; Example: (range 15 10)
;;    ==>   '()
(define (range a b)
  (if (< a b)
      (list* a (range (+ a 1) b))
      (list* '()))
)

;; Apply f to each element in lst and return the resulting list
;;
;; Example: (map (lambda (x) (* x 2)) '(1 2 3))
;;    ==>   '(2 4 6)
;;
;; Should behave equivalent to Racket's "map" function
(define (map f lst)
    (if (null? lst)
      (list* '())
      (if (null? (cdr lst))
          (cons (f (car lst)) '())
          (cons (f (car lst)) (map f (cdr lst))))
    )
)
;; Return a list of the elements x from lst for which (f x)
;; returns #t
;;
;; Example: (filter even? '(1 2 3))
;;    ==>   '(2)
;; Example: (filter (lambda (x) (= (remainder x 5) 1)) '(4 5 6 10 11 9))
;;    ==>   '(6 11)
;;
;; Should behave equivalent to Racket's "filter" function
(define (filter f lst)
  (if (null? lst)
      (list* '())
      (if (null? (cdr lst))
          (if (f (car lst))
              (cons((car lst) '()))
              (list* '()))
          (if (f (car lst))
              (list* (car lst) (filter f (cdr lst)))
              (filter f (cdr lst))))
  ))

;; Take a slice of n elements starting at index i in a list.
;;
;; Example: (slice-list 2 3 '(1 2 3 4 5 6))
;;    ==>   '(3 4 5)
;; Example: (slice-list 0 1 '(1 2 3))
;;    ==>   '(1)
;;
;; When there are not enough elements in the list, the behavior
;; is undefined. It is OK if your code returns either an error
;; or a value: we won't test these cases.
;;
;; define/match is optional for this function. feel free to change
;; to another form if you wish
(define/match (slice-list i n lst)
  [(_ _ _) (if (null? lst)
                (list* '())
                (if (= i 0)
                    (if (= n 0)
                        (list* '())
                        (list* (car lst) (slice-list i (- n 1) (cdr lst))))
                    (slice-list (- i 1) n (cdr lst))))
                ])

;; If elem is in lst, return the list starting at that point,
;; otherwise, return #f
;;
;; Example: (member 2 '(1 2 3))
;;    ==>   '(2 3)
;;
;; Example: (member 'x '(1 2 3))
;;    ==>   #f
;;
;; Should behave equivalent to Racket's "member" function.
(define (member elem lst)
  (if (null? lst)
      (eq? 0 1)
      (if (eq? elem (car lst))
          (if (null? (cdr lst))
              (list (car lst))
              (list* (car lst) (member (car (cdr lst)) (cdr lst))))
          (if (null? (cdr lst))
              (eq? 0 1)
              (member elem (cdr lst))))
  )
)

;; Merge takes two *sorted* lists and merges them into a single
;; sorted list. You may assume that both of these lists are of
;; integers.
;;
;; Example: (merge '(1 3 5 9) '(0 2 10))
;;    ==>   '(0 1 2 3 5 9 10)
;; Example: (merge '() '(1 2 3))
;;    ==>   '(1 2 3)
;; Example: (merge '(2 3 4) '())
;;    ==>   '(2 3 4)
;;
;; define/match is optional for this function. feel free to change
;; to another form if you wish
(define/match (merge a b)
  [(_ _) (if (null? a)
             (if (null? b)
                 (list* '())
                 (list* b))
             (if (null? b)
                 (list* a)
                 (if (< (car a) (car b))
                     (list* (car a) (merge (cdr a) b))
                     (list* (car b) (merge a (cdr b)))))
          )])

;; In mathematics, the fixpoint of a function f is where:
;;    f(x) = x
;; A function may have zero or more fixpoints. For example,
;;    f(x) = x^2
;; has fixpoints:
;;    f(0) = 0
;;    f(1) = 1
;;
;; Your job is to implement a function find-fixpoint, which takes
;; a function f (of a single floating-point argument), an floating
;; point number approx, and a function close-enough?. This function
;; should repeatedly apply f to approx until:
;;      (close-enough? (f approx) x) returns #t.
;;
;; In pseudocode, this looks like this:
;; procedure find-fixpoint (f, approx, close-enough?) (
;;     repeat (
;;         next-value <- f(approx)
;;         if close-enough?(next-value, approx) (
;;             return approx
;;         )
;;         approx <- next-value
;;     )
;; )
;;
;; Note that there are inputs which may make this function diverge.
;;
;; Example: (find-fixpoint cos 1.0 (lambda (a b) (< (abs (- a b)) 0.001)))
;;    ==>   0.7395672022122561
;; Example: (find-fixpoint sin 1.0 (lambda (a b) (< (abs (- a b)) 0.1)))
;;    ==>   0.8414709848078965
;; Example: (find-fixpoint (lambda (x) (* x x)) 0.9 (lambda (a b) (< (abs (- a b)) 0.0001)))
;;    ==>   1.390084523771456e-06
;; Example: (find-fixpoint (lambda (x) x) 0.1 (lambda (a b) (= a b)))
;;    ==>   0.1
(define (find-fixpoint f approx close-enough?)
  (if (close-enough? approx (f approx))
      approx
      (find-fixpoint f (f approx) close-enough?)
))
