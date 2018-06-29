#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; ** 1. sequence
(define (sequence low high stride)
  (if (>=  high low)
        (cons low (sequence (+ low stride) high stride))
        null))

; ** 2. string-append-map
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; ** 3. list-nth-mod
(define (list-nth-mod xs n)
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [(car (list-tail xs (remainder n (length xs))))]))

; ** 4. stream-for-n-steps
(define (stream-for-n-steps s n)
  (cond [(> n 0) (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]
        [cons (car (s)) null]))
; ** 5. funny-number-stream
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (* -1 x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; ** 6. dan-then-dog
(define dan-then-dog
  (letrec ([oppx (lambda(x) (if (equal? x "dan.jpg") "dog.jpg" "dan.jpg"))]
           [f (lambda (x) (cons x (lambda() (f (oppx x)))))])
    (lambda () (f "dan.jpg"))))

; ** 7. stream-add-zero
(define (stream-add-zero fn)
  (letrec ([f (lambda (x) 
                (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f fn))))

; ** 8. cycle-lists
(define (cycle-lists xs ys)
  (letrec ([f (lambda (x y) 
                (cons (cons (car x) (car y)) (lambda () (f (if (null? (cdr x)) xs (cdr x)) (if (null? (cdr y)) ys (cdr y)) ))))])
    (lambda () (f xs ys))))

; ** 9. 
(define (vector-assoc v vec)
  (letrec([f (lambda (x) (if (pair? x) (equal? v (car x)) #f))]
          [vec-len (vector-length vec)]
          [v-search (lambda(pos)
                    (if (< pos vec-len)
                        (if (f (vector-ref vec pos))
                            (vector-ref vec pos) (v-search (+ 1 pos))) ; inner if
                        #f)) ] ) ; letrec
    (v-search 0)  ))

; ** 10.
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [pos 0]
           [update-pos (lambda() (set! pos (if (equal? pos(- n 1)) 0 (+ 1 pos))))]
           [update-cache (lambda(x) (begin (vector-set! cache pos x) (update-pos)))]
           [search-cache (lambda(x) (vector-assoc x cache))]
           [fast-assoc (lambda (v)
                         (let([found (search-cache v)]
                              [costly-search (lambda()
                                               (let ([found2 (assoc v xs)])
                                                 (if found2 (begin (update-cache found2) found2) found2)) )]) ; let
                           (if found found (costly-search))) ; letrec2
                         )])
    (lambda (v) (fast-assoc v)) ) ; letrec
  ) ; define
