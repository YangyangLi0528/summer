;; Programming Languages, Homework 4

#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; Problem 3
(define (list-nth-mod xs n)
  (cond
    [(negative? n) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (list-ref xs (remainder n (length xs)))]))

;; Problem 4
(define (stream-for-n-steps s n)
  (if (zero? n)
      null
      (let ([next (s)])
        (cons (car next)
              (stream-for-n-steps (cdr next) (sub1 n))))))

;; Problem 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (zero? (remainder x 5)) (- x) x)
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; Problem 6
(define dan-then-dog
  (letrec ([f (lambda (x y)
                (lambda ()
                  (cons x (f y x))))])
    (f "dan.jpg" "dog.jpg")))

;; Problem 7
(define (stream-add-zero s)
  (lambda ()
    (let ([next (s)])
      (cons (cons 0 (car next))
            (stream-add-zero (cdr next))))))

;; Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (lambda ()
                  (cons (cons (list-nth-mod xs n)
                              (list-nth-mod ys n))
                        (f (add1 n)))))])
    (f 0)))
    

;; Problem 9
(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [f (lambda (i)
                (if (>= i l)
                    #f
                    (let ([elem (vector-ref vec i)])
                      (if (and (pair? elem) (equal? (car elem) v))
                          elem
                          (f (add1 i))))))])
    (f 0)))

;; Problem 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [pos 0]
           [f (lambda (v)
                (let loop ([i 0])
                  (if (= i n)
                      #f
                      (let ([entry (vector-ref cache i)])
                      (if (and (pair? entry) (equal? (car entry) v))
                          entry
                          (loop (add1 i)))))))])
    (lambda (v)
      (or (f v)
          (let ([res (assoc v xs)])
            (if res
                (begin
                  (vector-set! cache pos res)
                  (set! pos (remainder (add1 pos) n))
                  res)
                res))))))

;; Problem 11 - Challenge Problem
(define-syntax while-less
  (syntax-rules (to do)
    [(while-less e1 do e2)
     (let ([target e1])
       (let loop ([cur e2])
         (if (< cur target)
             (loop e2)
             #t)))]))
