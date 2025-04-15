#lang racket
(require "./lang.rkt" 
         "./event-loop.rkt"
         "./plist.rkt")
(provide run interp/async AsyncFunc AwaitC PromiseC AsyncFunV PromiseV AppC
         PromiseV? AsyncFunV? AsyncFunc? AwaitC? PromiseC? AppC?)

;; Extended AST nodes
(struct AsyncFunc (args body) #:transparent)
(struct AwaitC (expr) #:transparent)
(struct PromiseC (executor) #:transparent)
(struct AppC (fexpr args) #:transparent)

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
         [(FunV fenv argsym body)
          (let* ([p (promise evl 
                             (λ (resolve reject)
                               (let ([res-fun (FunV empty-env 'v (ApplyC (IdC 'resolve) (IdC 'v)))]
                                     [rej-fun (FunV empty-env 'v (ApplyC (IdC 'reject) (IdC 'v)))])
                                 (interp/async evl 
                                              (extend-env fenv '(resolve reject) (list res-fun rej-fun))
                                              mem1
                                              body))))])
            (values (PromiseV p) mem1))]
         [_ (error "Promise executor must be a function")]))]
    
    [(AwaitC expr)
     (let-values ([(promise-val mem1) (interp/async evl env mem expr)])
       (match promise-val
         [(PromiseV p)
          (let ([result-p (promise-then! p (λ (val) val))])
            (values (PromiseV result-p) mem1))]
         [_ (error "Await expects a promise")]))]
    
    ;; Handle our custom multi-argument AppC
    [(AppC fexpr args)
     (let*-values ([(fval mem1) (interp/async evl env mem fexpr)]
                   [(arg-vals mem2) (interp*/async evl env mem1 args)])
       (match fval
         [(AsyncFunV fenv fargs body)
          (let ([p (promise evl
                            (λ (resolve reject)
                              (evl-schedule evl (λ (_)
                                (let-values ([(result mem3) 
                                              (interp/async evl 
                                                          (extend-env fenv fargs arg-vals)
                                                          mem2
                                                          body)])
                                  (resolve result))))))])
            (values (PromiseV p) mem2))]        
         [(FunV fenv argsym body)
          (if (symbol? argsym) 
              ;; Classic function with single argument
              (interp (extend fenv argsym (car arg-vals)) mem2 body)
              ;; Assuming list args aren't supported in base lang.rkt
              (error "Function argument mismatch"))]
         [_ (error "Cannot apply non-function")]))]
    
    ;; Handle standard single-argument ApplyC from lang.rkt
    [(ApplyC fexpr vexpr)
     (let*-values ([(fval mem1) (interp/async evl env mem fexpr)]
                   [(vval mem2) (interp/async evl env mem1 vexpr)])
       (match fval
         [(AsyncFunV fenv fargs body)
          (if (= (length fargs) 1)
              (let ([p (promise evl
                                (λ (resolve reject)
                                  (evl-schedule evl (λ (_)
                                    (let-values ([(result mem3) 
                                                  (interp/async evl 
                                                              (extend fenv (car fargs) vval)
                                                              mem2
                                                              body)])
                                      (resolve result))))))])
                (values (PromiseV p) mem2))
              (error "AsyncFunc expected ~a args but got 1" (length fargs)))]
         [(FunV fenv argsym body)
          (interp (extend fenv argsym vval) mem2 body)]
         [_ (error "Cannot apply non-function")]))]
         
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
           (promise evl (λ (resolve reject) (resolve result))))))))