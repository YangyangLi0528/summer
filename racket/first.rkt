#lang racket
; make it public for test
(provide (all-defined-out))
(define s "hello") ;this is another comment
(define x 3) ; val x = 3

; (define x e) x is a variable and e is an expression,we evaluate e to a value
; and change the environment so that x is bound to that value
(define y (+ x 2)) ; + is a function,call it here

; a anonymous function that takes one argument is written (lambda (x) e)
; where the argument is the variable x and the body is the expression e
(define cube1
  (lambda (x)
    (* x(* x x))))

; many functions can take any number of arguments, *,is one of them
(define cube2
  (lambda (x)
    (* x x x)))

(define (cube3 x)
  (* x x x))

; you can use recursion with anonymous functions because the definition itself
; is in scope in the function body
(define pow
  (lambda (x y)
    (if(= y 0)
       1
       (* x (pow x (- y 1))))))

(define (pow1 x y)
  (if (= y 0)
      1
      (* x  (pow1 x (- y 1)))))


;use currying in Racket
(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))


(define (sum1 xs)
  (if (null? xs)
      0
      (+ (car xs) (sum1 (cdr xs)))))

(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs)(my-append(cdr xs)ys))))

(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (my-map f(cdr xs)))))

; sum for (list 2 (list 4 5) (list (list 1 2) (list 5)) 19 (list 12 9))
(define (sum2 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs)(sum2 (cdr xs)))
          (+ (sum2 (car xs))(sum2 (cdr xs))))))

; the cond special form,which is better style for nested conditionals
; than actually using multiple if-expressions
(define (sum3 xs)
  (cond [(null? xs)0]
        [(number? (car xs))(+ (car xs)(sum3 (cdr xs)))]
        [#t (+ (sum3 (car xs)) (sum3 (cdr xs)))]))

; suppose we even want to allow non-numbers and non lists in our list
(define (sum4 xs)
  (cond [(null? xs) 0]
        [(number? xs) xs]
        [(list? xs) (+ (sum4 (car xs))(sum4 (cdr xs)))]
        [#t 0]))

;Local bindings: let, let*, letrec, local define
;What environment do we use to evaluate e1, e2, ..., en? It
;turns out we use the environment from “before” the let-expression
;If e3 uses x1 or x2, that would either be an error or would mean some
;outer variable of the same name.
(define (silly-double-1 x)
  (let ([x (+ x 3)]
        [y (+ x 2)])
    (+ x y -5)))

(define (silly-double-2 x)
  (let* ([x (+ x 3)]
         [y (+ x 2)])
    (+ x y -8)))

; let let* can not use recursion
; letrec can define one or more (mutually) recursive functions
(define (triple x)
  (letrec ([y (+ x 2)]
           [f (lambda (z) (+ z y w x))]
           [w (+ x 7)])
    (f -9)))
;One typically uses letrec to define one or more (mutually) recursive functions, such as this very slow method
;for taking a non-negative number mod 2    
(define (mod2 x)
  (letrec
      ([even? (lambda (x) (if (zero? x) #t (odd? (- x 1))))]
       [odd? (lambda (x) (if (zero? x) #f (even? (- x 1))))])
    (if(even? x)0 1)))

; Alternately, you can get the same behavior as letrec by using local defines
(define (mod2_b x)
  (define even? (lambda(x)(if (zero? x) #t (odd? (- x 1)))))
  (define odd? (lambda(x)(if (zero? x) #f (even? (- x 1)))))
  (if (even? x) 0 1))

    

              