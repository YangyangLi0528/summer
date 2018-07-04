;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
; Write a Racket function racketlist->mupllist that takes a Racket list (presumably of mupl
; values but that will not affect your solution) and produces an analogous mupl list with the same
; elements in the same order
(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

; Write a Racket function mupllist->racketlist that takes a mupl list (presumably of mupl
; values but that will not affect your solution) and produces an analogous Racket list (of mupl
; values) with the same elements in the same order.
(define (mupllist->racketlist mxs)
  (if (aunit? (apair-e2 mxs))
      (cons (apair-e1 mxs) null)
      (cons (apair-e1 mxs) (mupllist->racketlist (apair-e2 mxs)))))
      

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]        
        [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]        
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env) 
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater given non-number in first two arguments")))]
        [(mlet? e)
         (if (string? (mlet-var e))
             (let* ([v1 (eval-under-env (mlet-e e) env)]
                    [env-extended (cons (cons (mlet-var e) v1) env)])
               (eval-under-env (mlet-body e) env-extended))
             (error "MUPL mlet given non-string in first argument"))]        
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]               
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([f (closure-fun v1)] 
                      [v1-env (closure-env v1)]
                      [f-name (fun-nameopt f)]
                      [fenv-temp (cons (cons (fun-formal f) v2) v1-env)] ; add the function argumentn to env
                      [fenv (if f-name                                   ; if the function is named, add it to env
                                (cons (cons f-name v1) fenv-temp)
                                fenv-temp)])
                 (eval-under-env (fun-body f) fenv))  ; evaluate function body with the extended environment
               (error "MUPL call given a non-closure as first input")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]                  
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "MUPL fst given a non-pair as input")))]
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "MUPL snd given a non-pair as input")))]
        [(isaunit? e)
         (let ([v1 (eval-under-env (isaunit-e e) env)])
           (if (aunit? v1)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))


;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)  
  (let ([body (lambda(lstlst) (if (null? (cdr lstlst)) ; if there is only one variable left 
                                e2                     ; then the body of the next mlet call will be e2
                                (mlet* (cdr lstlst) e2)))]  ; otherwise recursively call mlet*
        [v-name (car (car lstlst))]
        [e (cdr (car lstlst))])
    (mlet v-name e (body lstlst))))
  
  

(define (ifeq e1 e2 e3 e4)
  ; if e1 and e2 are equal then evaluate e3 otherwise e4
  (mlet "v1" e1 ; use mlet so e1 is only evaluated once
        (mlet "v2" e2 ; use mlet so e2 is only evaluated once
              (ifgreater (add (var "v1") (int 1))  ; check if v1+1 is bigger then v2
                         (var "v2")
                         (ifgreater (add (var "v2") (int 1)) ; check if v2+1 is bigger than v1                                    
                                    (var "v1")   
                                    e3
                                    e4)
                         e4))))
        

;; Problem 4

(define mupl-map
  (fun #f "f"  ;This  mupl function should take a function and return a function                
       (fun "mmap" "mxs"  ; This mupl function should take a list a return a list
            (ifaunit (var "mxs")
                     (aunit) ; if all that is left in the list is a (aunit), just return (aunit)
                     (apair  ; otherwise, call "f" on the first argument of the list and recursively call on the second argument
                      (call (var "f") (fst (var "mxs")))                               
                      (call (var "mmap") (snd (var "mxs"))))))))

                
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))
        

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment

; needs to recursively traverse e returning the same thing except when there is a function needs
; to return fun-challenge with the freevars stored
(define (compute-free-vars e) ;"CHANGE")
  (let ([find-free-vars (lambda (body formalset)
                          (letrec ([f (lambda (e freeset)
                                     (cond [(or (int? e) (aunit? e)) freeset]
                                           [(var? e) (if (set-member? formalset (var-string e))
                                                         freeset
                                                         (set-add freeset (var-string e)))]
                                           [(add? e) (set-union freeset
                                                                (f (add-e1 e) freeset)
                                                                (f (add-e2 e) freeset))]
                                           ; need to add all other cases here. Then need to write eval-under-env-c
                                           ))])
                            (f body (set))))])
                                           
                                                      
                                           ;[(closure? e)
                                           
                                           
                        
    (cond [(int? e) e]
          [(closure? e) (closure (closure-env e) (compute-free-vars (closure-fun e)))]
          [(aunit? e) e]        
          [(var? e) e]
          [(add? e) (add (compute-free-vars (add-e1 e)) (compute-free-vars (add-e2 e)))]        
          [(fun? e) (fun-challenge (fun-nameopt e)
                                   (fun-formal e)
                                   (fun-body e)
                                   (find-free-vars (fun-body e) (set (fun-formal e))))]
          [(ifgreater? e) (ifgreater (compute-free-vars (ifgreater-e1 e))
                                     (compute-free-vars (ifgreater-e2 e))
                                     (compute-free-vars (ifgreater-e3 e))
                                     (compute-free-vars (ifgreater-e4 e)))]
          [(mlet? e) (mlet (compute-free-vars (mlet-var e))
                           (compute-free-vars (mlet-e e))
                           (compute-free-vars (mlet-body e)))]
          [(call? e) (call (compute-free-vars (call-funexp e))
                           (compute-free-vars (call-actual e)))]         
          [(apair? e) (apair (compute-free-vars (apair-e1 e)) (compute-free-vars (apair-e2 e)))]
          [(fst? e) (fst (compute-free-vars e))]
          [(snd? e) (snd (compute-free-vars e))]
          [(isaunit? e) (isaunit (compute-free-vars e))]         
          [#t (error (format "bad MUPL expression: ~v" e))])))


  

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
