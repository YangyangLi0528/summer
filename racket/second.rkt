#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define (funny-sum xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs)(funny-sum (cdr xs)))]
        [(string? (car xs)) (+ (string-length (car xs))(funny-sum (cdr xs)))]
        [#t (error "expected number of string")]))

;helper functions for constructing
(define (Const i)(list 'Const i))
(define (Negate e)(list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2))
(define (Multiply e1 e2) (list 'Multiply e1 e2))
; helper functions for testing
(define (Const? x) (eq? (car x) 'Const))
(define (Negate? x) (eq? (car x) 'Negate))
(define (Add? x) (eq? (car x) 'Add))
(define (Multiply? x) (eq? (car x) 'Multiply))
; helper functions for accessing
(define (Const-int e) (car (cdr e)))
(define (Negate-e e) (car (cdr e)))
(define (Add-e1 e) (car (cdr e)))
(define (Add-e2 e) (car (cdr (cdr e))))
(define (Multiply-e1 e) (car (cdr e)))
(define (Multiply-e2 e) (car (cdr (cdr e))))

; recursive datatypes via racket lists
(define (eval-exp e)
  (cond [(Const? e) e] ; returning an exp,not a number
        [(Negate? e)(Const (- (Const-int (eval-exp (Negate-e e)))))]
        [(Add? e)(let ([v1 (Const-int (eval-exp (Add-e1 e)))]
                       [v2 (Const-int (eval-exp (Add-e2 e)))])
                   (Const (+ v1 v2)))]
        [(Multiply? e)(let ([v1 (Const-int (eval-exp (Multiply-e1 e)))]
                            [v2 (Const-int (eval-exp (Multiply-e2 e)))])
                        (Const (* v1 v2)))]
        [#t (error "eval-exp expected an exp")]))

(define test-exp (Multiply (Negate (Add (Const 2)(Const 2)))(Const 7)))
(define test-ans (eval-exp test-exp))

; recursive datatypes via racket struct
; a struct definition looks like:
; (struct foo (bar baz quux)#:transparent)
; this defines a new struct called foo,it adds to the environment functions for constructing a foo
; test if something is a foo,
; and extracting the fields bar,baz,quux from a foo.

; foo is a function that takes three arguments and returns a value that is a foo with a bar field
; holding the first argument,a baz field holding the second argument,and a quux field holding the
; third argument

; foo-bar is a function that takes a foo and return the contents of the bar field

; the #:transparent attribute makes the fields and accessor functions visible even outside the module that defines the struct
; the #:mutable attribute makes all fields mutable by also providing mutator functions 

(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)

(define (eval-exp-2 e)
  (cond [(const? e) e] ; returning an exp not a number
        [(negate? e)(const (- (const-int (eval-exp-2 (negate-e e)))))]
        [(add? e)(let ([v1 (const-int (eval-exp-2 (add-e1 e)))]
                       [v2 (const-int (eval-exp-2 (add-e2 e)))])
                   (const (+ v1 v2)))]
        [(multiply? e)(let ([v1 (const-int (eval-exp (multiply-e1 e)))]
                            [v2 (const-int (eval-exp (multiply-e2 e)))])
                        (const (* v1 v2)))]
        [#t (error "eval-exp expected an exp")]))



(struct bool (b) #:transparent)
(struct if-then-else (e1 e2 e3) #:transparent)
(struct eq-num (e1 e2) #:transparent)





        