
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
      (cons (car s) (stream-for-n-steps (cdr s) (- n -1)))))

; 5 funny-number-stream



