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
; conclusion:
; letrec and local define allow code to refer to variables
; that are initialized later,but the expressions for each binding still evaluated in order

; The semantics for letrec requires that the use of z for initializing y refers to the z in the letrec, but
; the expression for z (the 13) has not been evaluated yet. In this situation, Racket will raise an error when
; bad-letrec is called. (Prior to Racket Version 6.1, it would instead bind y to a special “undefined” object,
; which almost always just had the effect of hiding a bug
(define (bad-letrec x)
  (letrec ([y z]
           [z 13])
    (if x y z)))


; Bindings are generally mutable---use set!
; Doing so affects all code that has this x in its environment.
(define b 3)
(define f (lambda (x) (* 1 (+ x b))))
(define c (+ b 4))
(set! b 5)
(define z (f 4))
(define w c)

; This code makes the b in the function body refer to
; a local b that is initialized to the global b.
(define f2
  (let ([b b])
    (lambda (x) (* 1 (+ x b)))))

; delayed evaluation and thunks

; (define (my-if-bad x y z) (if x y z))
; (define (factorial-wrong x)
;   (my-if-bad (= x 0)
;              1
;              (* x (factorial-wrong(- x 1)))))

(define (my-if x y z)(if x (y) (z)))

(define (factorial x)
  (my-if (= x 0)
         (lambda () 1)
         (lambda () (* x (factorial (- x 1))))))

; lazy evaluation with delay and force
(define (my-delay f)
  (mcons #f f))

(define (my-force th)
  (if (mcar th)
      (mcdr th)
      (begin (set-mcar! th #t)
             (set-mcdr! th ((mcdr th)))
             (mcdr th))))

; multiply the result of two expressions e1 and e2 using arecursive algorithm
; Now calling (my-mult e1 e2) evaluates e1 and e2 once each and then does 0 or more additions
; But what if e1 evaluates to 0 and e2 takes a long time to compute?
; Then evaluating e2 was wasteful
(define (my-mult x y)
  (cond [(= x 0) 0]
        [(= x 1) y]
        [#t (+ y (my-mult (- x 1) y))]))

; thunk it
; Now we would call (my-mult e1 (lambda () e2))
(define (my-mult-thunk x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk)(my-mult-thunk (- x 1) y-thunk))]))

; let’s use my-delay and my-force to get the best of both worlds:
; (my-mult-thunk e1 (let ([x (my-delay (lambda () e2))]) (lambda () (my-force x))))

  
; stream
(define ones
  (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))
; function that takes a stream and a predicate-function and returns how many
; stream elements are produced before the predicate-function returns true
(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      ans
                      (f (cdr pr)(+ ans 1)))))])
    (f stream 1)))
; (number-until powers-of-two (lambda (x) (= x 16)))

; all the streams above can produce their next element given at most their previous element
; higher order functions to reuse common functionality
(define (stream-maker fn arg)
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))

(define ones- (stream-maker (lambda (x y) 1) 1))
(define nats- (stream-maker + 1))
(define powers-of-two- (stream-maker * 2))  
    
  
; Memoization
(define (fibonacci_1 x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibonacci_1 (- x 1))
         (fibonacci_1 (- x 2)))))

; taking a “count up” approach
;that remembers previous answers:
(define (fibonacci_2 x)
  (letrec ([f (lambda (acc1 acc2 y)
                (if (= y x)
                    (+ acc1 acc2)
                    (f (+ acc1 acc2) acc1 (+ y 1))))])
    (if (or (= x 1)(= x 2))
        1
        (f 1 1 3))))

; use assoc library
(define fibonacci
  (letrec ([memo null]
           [f (lambda (x)
                (let ([ans (assoc x memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= x 1)(= x 2))
                                         1
                                         (+ (f (- x 1))
                                            (f (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans) memo))
                          new-ans)))))])
    f))

; Macros:the key points

              
  


              