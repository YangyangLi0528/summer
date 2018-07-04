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

;; CHANGE (put your solutions here)
(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (apair (car lst) (racketlist->mupllist (cdr lst)))))

(define (mupllist->racketlist e)
  (cond [(aunit? e) null]
        [ (apair? e) (cons (apair-e1 e) (mupllist->racketlist (apair-e2 e)))]))

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
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        ;; case int (num) integer
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        ;; case fun  (nameopt formal body) ; a recursive(?) 1-argument function
        [(fun? e)
         (let ([fname (fun-nameopt e)])
           (closure (if fname (cons (cons fname (closure env e)) env) env) e))]
        ;; case ifgreater (e1 e2 e3 e4) ; greater than (if e1 > e2 then e3 else e4)
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL greather than applied to non-number")))]
        ;; case mlet (var e body) ; local binding  (let var = e in body)
        [(mlet? e)
         (letrec ([v (eval-under-env (mlet-e e) env)]
                  [env_ (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) env_))]
        ;; case call (funexp actual) ; function call
        [(call? e)
         (letrec ([clos_ (eval-under-env (call-funexp e) env)])
           (if (closure? clos_)
               (let* ([arg_ (eval-under-env (call-actual e) env)]
                     [env_ (closure-env clos_)]
                     [fct_ (closure-fun clos_)]
                     [fctname_ (fun-nameopt fct_)]
                     [env__ (if fctname_
                                 (cons (cons fctname_ clos_) (cons (cons (fun-formal fct_) arg_) env_))
                                 (cons (cons (fun-formal fct_) arg_) env_))])
                 (eval-under-env (fun-body fct_) env__))
               (error "MUPL call 1st subexpression not a closure")))]
        ;; case apair (e1 e2); make a new pair
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        ;; case fst (e) ; get first part of a pair
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               ("error MUPL fst argument nor an apair struct")))]
        ;; case snd (e) ; get second part of a pair
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               ("error MUPL snd argument nor an apair struct")))]
        ;; case isaunit (e) ; evaluate to (int 1) if e is unit else (int 0)
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([bidding_ (car lstlst)]
            [lstlst_next (cdr lstlst)])
        (mlet (car bidding_) (cdr bidding_) (mlet* lstlst_next e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
    (ifgreater (var "_x") (var "_y") e4
               (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "fct"
       (fun "fHelp" "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair 
                      (call (var "fct") (fst (var "lst")))
                      (call (var "fHelp") (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun "fHelp" "x" (add (var "i") (var "x")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
