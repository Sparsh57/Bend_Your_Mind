#lang racket

(require "./lang.rkt" 
         "./event-loop.rkt"
         "./plist.rkt")
(require "lang.rkt")
;; Extended AST nodes
(struct AsyncFunc (args body) #:transparent)
(struct AwaitC (expr) #:transparent)
(struct PromiseC (executor) #:transparent)

;; Extended value types
(struct AsyncFunV (env args body) #:transparent)
(struct PromiseV (p) #:transparent)

;; Helper to extend environment with multiple arguments
(define (extend-env env args vals)
  (foldl (λ (arg val env) (extend env arg val)) env args vals))

;; Async interpreter
(define (interp/async evl env mem expr)
  (match expr
    [(AsyncFunc args body)
     (values (AsyncFunV env args body) mem)]

    [(PromiseC executor-expr)
     (let-values ([(executor-val mem1) (interp/async evl env mem executor-expr)])
       (match executor-val
         [(FunV _ argsym body) #:when (equal? argsym '(resolve reject))
          (let* ([p (promise evl (λ (resolve reject)
                        (let* ([resolve-fun (FunV env 'resolve (λ (v) (resolve v)))]
                               [reject-fun (FunV env 'reject (λ (v) (reject v)))])
                          (interp/async evl 
                                       (extend-env env '(resolve reject) (list resolve-fun reject-fun))
                                       mem1
                                       body))))])
            (values (PromiseV p) mem1))]
         [_ (error "Promise executor must be a function (resolve, reject) => ...")]))]

    [(AwaitC expr)
     (let-values ([(promise-val mem1) (interp/async evl env mem expr)])
       (match promise-val
         [(PromiseV p)
          (let ([result-p (promise-then! p (λ (val) val))])
            (values (PromiseV result-p) mem1))]
         [_ (error "Await expects a promise")]))]

    [(ApplyC fexpr args)
     (let*-values ([(fval mem1) (interp/async evl env mem fexpr)]
                   [(arg-vals mem2) (interp*/async evl env mem1 args)])
       (match fval
         [(AsyncFunV fenv fargs body)
          (let ([p (promise evl (λ (resolve reject)
                       (evl-schedule evl (λ (_)
                         (let-values ([(result mem3) 
                                       (interp/async evl 
                                                     (extend-env fenv fargs arg-vals)
                                                     mem2
                                                     body)])
                           (resolve result))))))]) 
            (values (PromiseV p) mem2))]        
         [_ (interp/async evl env mem (ApplyC fexpr args))]))] 

    [_ (interp env mem expr)])) 

;; Helper for multiple expressions
(define (interp*/async evl env mem exprs)
  (if (null? exprs)
      (values '() mem)
      (let*-values ([(v mem1) (interp/async evl env mem (car exprs))]
                   [(vs mem2) (interp*/async evl env mem1 (cdr exprs))])
        (values (cons v vs) mem2))))

;; Run function
(define (run expr)
  (run-in-event-loop
   (λ (evl)
     (let-values ([(result mem) (interp/async evl empty-env '() expr)])
       (if (PromiseV? result)
           (PromiseV-p result)
           (promise evl (λ (resolve _) (resolve result))))))))


