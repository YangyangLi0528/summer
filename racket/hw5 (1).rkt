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

;; a pair is also not in "source" programs but /is/ a MUPL value; it is what apairs evaluate to
(struct pair (v1 v2) #:transparent)

;; Problem 1

(define (racketlist->mupllist rls)
  (if (null? rls)
      (aunit)
      (apair (car rls) (racketlist->mupllist (cdr rls)))))

(define (mupllist->racketlist mls)
  (if (aunit? mls)
      null
      (cons (apair-e1 mls) (mupllist->racketlist (apair-e2 mls)))))
  

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
        [(or (int? e) (closure? e) (aunit? e) (pair? e)) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (eval-under-env ((if (> (int-num v1) (int-num v2))
                                    ifgreater-e3
                                    ifgreater-e4)
                                e)
                               env)
               (error "MUPL ifgreater applied to non-number")))]
        [(fun? e) ;; nameopt formal body
         (closure env e)]
        [(call? e) ;; funexp actual
         (let* ([clos (eval-under-env (call-funexp e) env)]
                [actual (eval-under-env (call-actual e) env)]
                [clos-fun (closure-fun clos)])
           (eval-under-env (fun-body clos-fun)
                           (cons (cons (fun-formal clos-fun) actual)
                                 (closure-env clos))))]
        [(mlet? e)
         (let ([vname (mlet-var e)]
               [e1 (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons vname e1) env)))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (pair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (pair? v)
               (pair-v1 v)
               (error "MUPL fst applied to non-pair")))]

        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (pair? v)
               (pair-v2 v)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (int (if (aunit? v) 1 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0)
             e2
             e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([bnd1 (car lstlst)]
            [lstlst1 (cdr lstlst)])
        (mlet (car bnd1) (cdr bnd1) (mlet* lstlst1 e2)))))

(define (ifeq e1 e2 e3 e4)
  (let ([var1 (symbol->string (gensym "var1"))]
        [var2 (symbol->string (gensym "var2"))])
    (mlet* (list (cons var1 e1) (cons var2 e2))
           (ifgreater (var var1) (var var2)
                      e4
                      (ifgreater (var var2) (var var1)
                                 e4
                                 e3)))))

;; Problem 4
(define mupl-map
  (let ([mapfn (symbol->string (gensym "mapfn"))])
    (fun #f "f"
         (fun mapfn "lst"
              (ifaunit (var "lst")
                       (var "lst")
                       (apair (call (var "f") (fst (var "lst")))
                              (call (var mapfn) (snd (var "lst")))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

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
