
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1 sequence
(define sequence
  (lambda (low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride)))))
  
; 2 string-append-map
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; 3 list-nth-mod xs n
; use nested if
;(define (list-nth-mod xs n)
;  (if (< n 0)
;      (error "list-nth-mod: negative number")
;      (if (null? xs)
;          (error "list-nth-mod: empty list")
;          (car (list-tail xs (remainder n (length xs)))))))
; use cond
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs)(error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; 4 stream-for-n-steps
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; 5 funny-number-stream
(define funny-number-stream
  (letrec ([f (lambda (n)
              (cond [(< n 0)(cons n (lambda () (f (+ (* n -1) 1))))]
                    [(= (remainder (+ n 1) 5) 0) (cons n (lambda () (f (* (+ n 1) -1))))]
                    [#t (cons n (lambda () (f (+ n 1))))]))])
    (lambda () (f 1))))

; 6 dan-then-dog
; use letrec recursion
(define dan-then-dog
  (letrec ([f (lambda () (cons "dan.jpg" (lambda () (g))))]
           [g (lambda () (cons "dog.jpg" (lambda () (f))))])
    (lambda () (f))))

; use local defines
; error:bad syntax (multiple expressions after identifier)
; how to fix it.
;(define dan-then-dog
;  (define f (lambda () (cons "dan.jpg" (lambda () (g)))))
;  (define g (lambda () (cons "dog.jpg" (lambda () (f)))))
;  (lambda () (f)))

; 7 stream-add-zero
(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (cons
                 (cons 0 (car (s)))
                 (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

; 8 cycle-lists
; Use list-nth-mod: takes a list xs and a number n
; return the i~th element of the list; i is the remainder produced when dividing n by the list's length 
; Use a recursive helper function that takes a number n and calls itself with (+ n 1) inside a thunk
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

; 9 vector-assoc
; like racket's assoc function---look for a value in a list
; (vector-ref vec pos): Returns the element in slot pos of vec
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                ;return #f if no vector element is a pair with a car field equal to v
                (if (= n (vector-length vec))
                    #f
                    ; else return the first pair with an equal car field
                    (let ([element (vector-ref vec n)])
                      ; allows vector elements not to be pairs in which case it skips them
                      (cond [(not (pair? element)) (f (+ n 1))]
                            [(equal? (car element) v) element]
                            (#t (f (+ n 1)))))))])
    (f 0)))

; 10 cached-assoc
(define (cached-assoc xs n)
  (letrec ([index 0]
           [cache (make-vector n #f)]
           [f (lambda(v)
                (let ([element (vector-assoc v cache)])
                  (if element
                      element
                      (let ([value (assoc v xs)])
                        (vector-set! cache index value)
                        (set! index (remainder (+ index 1) n))
                        value))))])
    f))
                
                
           
                             
                      
