#lang racket

(require racket/match)
(require racket/trace)
(require "./plist.rkt")

(provide expr? exprC? exprS? 
         desugar interp interpS
         extend empty-env lookup
         NumC AddC MulC InvC LtC EqC 
         TrueC FalseC AndC OrC NotC IfC
         SubS GtS
         IdC FunC ApplyC 
         BoxC UnboxC SetBoxC SeqC
         NumV BoolV FunV BoxV)

(define pfind plist-find)
(define passert plist-assert)
(define psat? plist-sat?)

(define make-addr
  (let ([addrs (mutable-set)])
    (λ ()
      (let loop ([addr (random 4000000000)])
        (if (set-member? addrs addr)
            (loop (random 4000000000))
            (begin (set-add! addrs addr)
                   addr))))))
            
        
;                                                        
;                                                        
;                                                        
;     ;;;;                                    ;          
;    ;    ;                                   ;          
;    ;                                        ;          
;   ;        ;     ;    ;;;;     ; ;;;    ;;; ;   ;;;;;  
;   ;        ;     ;   ;    ;    ;;   ;  ;   ;;  ;     ; 
;   ;   ;;;  ;     ;        ;    ;      ;     ;  ;       
;   ;     ;  ;     ;   ;;;;;;    ;      ;     ;  ;;;;    
;   ;     ;  ;     ;  ;;    ;    ;      ;     ;     ;;;; 
;    ;    ;  ;     ;  ;     ;    ;      ;     ;        ; 
;    ;    ;  ;;   ;;  ;    ;;    ;       ;   ;;  ;     ; 
;     ;;;;    ;;;; ;   ;;;; ;    ;        ;;; ;   ;;;;;  
;                                                        
;                                                        
;                                                        
;                                                        

(define g/real (λ (n name)
                 (unless (real? n)
                   (error name "Number must be real, but got ~s" n))
                 n))

(define g/expr (λ (n name)
                 (unless (expr? n)
                   (error name "Number must be expr, but got ~s" n))
                 n))

(define g/2exprs (λ (a b name)
                   (unless (expr? a)
                     (error name "First argument ~s is not an expression" a))
                   (unless (expr? b)
                     (error name "Second argument ~s is not an expression" a))
                   (values a b)))



;                                      
;                                      
;                                      
;     ;;;;                             
;    ;    ;                            
;    ;                                 
;   ;          ;;;      ; ;;;   ;;;;   
;   ;         ;   ;     ;;   ;  ;   ;  
;   ;        ;     ;    ;      ;    ;; 
;   ;        ;     ;    ;      ;;;;;;; 
;   ;        ;     ;    ;      ;       
;    ;       ;     ;    ;      ;       
;    ;    ;   ;   ;     ;       ;      
;     ;;;;     ;;;      ;        ;;;;  
;                                      
;                                      
;                                      
;                                      

(struct NumC (n) #:transparent
  #:guard g/real)

(struct AddC (a b) #:transparent
  #:guard g/2exprs)

(struct MulC (a b) #:transparent
  #:guard g/2exprs)

(struct InvC (e) #:transparent
  #:guard g/expr)

(struct LtC (e1 e2) #:transparent #:guard g/2exprs)
(struct EqC (e1 e2) #:transparent #:guard g/2exprs)

(struct TrueC () #:transparent)
(struct FalseC () #:transparent)
(struct AndC (a b) #:transparent #:guard g/2exprs)
(struct OrC (a b) #:transparent #:guard g/2exprs)
(struct NotC (b) #:transparent #:guard g/expr)
(struct IfC (cond then else) #:transparent
  #:guard (λ (bool thenexpr elseexpr name)
            (g/expr bool name)
            (g/2exprs thenexpr elseexpr name)
            (values bool thenexpr elseexpr)))


(struct FunC (argsym body) #:transparent
  #:guard (λ (argsym body name)
            (unless (symbol? argsym)
              (error "Invalid arg symbol"))
            (unless (expr? body)
              (error "body is not an expression"))
            (values argsym body)))

(struct IdC (id) #:transparent
  #:guard (λ (id name)
            (unless (symbol? id)
              (error "Identifier must be a symbol"))
            id))

(struct ApplyC (fexpr vexpr) #:transparent
  #:guard g/2exprs)

(struct BoxC (expr) #:transparent #:guard g/expr)
(struct UnboxC (bexpr) #:transparent #:guard g/expr)
(struct SetBoxC (bexpr vexpr) #:transparent #:guard g/2exprs)

; Supports a sequence of expressions much like (begin ...)
; The result value of SeqC is the value of the last expression
; in the list.
(struct SeqC (exprs) #:transparent
  #:guard (λ (exprs name)
            (unless (and (list? exprs) (> (length exprs) 0))
              (error 'SeqC "Must be a non empty list of exprs. Got ~s" exprs))
            (let loop ([exprs exprs])
              (unless (empty? exprs)
                (unless (expr? (first exprs))
                  (error 'SeqC "Each entry in list must be an expr. Got ~s" (first exprs)))
                (loop (rest exprs))))
            exprs))

;                                               
;                                               
;                                               
;     ;;;;                                      
;   ;;   ;;                                     
;   ;                                           
;   ;        ;     ;    ;;; ;    ;;;;     ; ;;; 
;   ;;       ;     ;   ;   ;;   ;    ;    ;;   ;
;     ;;;    ;     ;  ;     ;        ;    ;     
;        ;;  ;     ;  ;     ;   ;;;;;;    ;     
;         ;  ;     ;  ;     ;  ;;    ;    ;     
;   ;     ;  ;     ;  ;     ;  ;     ;    ;     
;   ;;   ;;  ;;   ;;   ;   ;;  ;    ;;    ;     
;    ;;;;;    ;;;; ;    ;;; ;   ;;;; ;    ;     
;                           ;                   
;                      ;   ;;                   
;                       ;;;;                    
;                                               

(struct SubS (a b) #:transparent
  #:guard g/2exprs)

(struct GtS (e1 e2) #:transparent #:guard g/2exprs)

(define (expr? e)
  (or (exprC? e)
      (exprS? e)))

(define (exprC? e)
  (or (NumC? e)
      (AddC? e)
      (MulC? e)
      (InvC? e)
      (IdC? e)
      (FunC? e)
      (ApplyC? e)
      (LtC? e)
      (EqC? e)
      (TrueC? e)
      (FalseC? e)
      (IfC? e)
      (AndC? e)
      (OrC? e)
      (NotC? e)
      (BoxC? e)
      (UnboxC? e)
      (SetBoxC? e)
      (SeqC? e)))

(define (exprS? e)
  (or (SubS? e)
      (GtS? e)))



;                                                        
;                                                        
;                                                        
;   ;     ;            ;;;;                              
;   ;     ;               ;                              
;    ;   ;                ;                              
;    ;   ;     ;;;;       ;    ;     ;   ;;;;     ;;;;;  
;    ;   ;    ;    ;      ;    ;     ;   ;   ;   ;     ; 
;    ;   ;         ;      ;    ;     ;  ;    ;;  ;       
;     ; ;     ;;;;;;      ;    ;     ;  ;;;;;;;  ;;;;    
;     ; ;    ;;    ;      ;    ;     ;  ;           ;;;; 
;     ; ;    ;     ;      ;    ;     ;  ;              ; 
;     ; ;    ;    ;;      ;    ;;   ;;   ;       ;     ; 
;      ;      ;;;; ;       ;;;  ;;;; ;    ;;;;    ;;;;;  
;                                                        
;                                                        
;                                                        
;                                                        

(struct NumV (n) #:transparent
  #:guard g/real)

(struct BoolV (b) #:transparent
  #:guard (λ (b name)
            (unless (boolean? b)
              (error name "Expecting boolean, got ~s" b))
            b))

(struct FunV (env argsym body) #:transparent
  #:guard (λ (env argsym body name)
            (unless (symbol? argsym)
              (error "Invalid arg symbol"))
            (unless (expr? body)
              (error "body is not an expression"))
            (values env argsym body)))

(define boxvalkey (gensym 'boxvalkey))
(struct BoxV (addr) #:transparent)

;                                                        
;                                                        
;       ;                                                
;       ;                                                
;                       ;                                
;                       ;                                
;     ;;;    ; ;;;;   ;;;;;;    ;;;;      ; ;;;  ; ;;;   
;       ;    ;;   ;;    ;       ;   ;     ;;   ; ;;   ;  
;       ;    ;     ;    ;      ;    ;;    ;      ;     ; 
;       ;    ;     ;    ;      ;;;;;;;    ;      ;     ; 
;       ;    ;     ;    ;      ;          ;      ;     ; 
;       ;    ;     ;    ;      ;          ;      ;     ; 
;       ;    ;     ;    ;       ;         ;      ;;   ;  
;    ;;;;;;; ;     ;     ;;;     ;;;;     ;      ; ;;;   
;                                                ;       
;                                                ;       
;                                                ;       
;                                                        

(define (lookup env id)
  (env id))

(define (extend env id val)
  (λ (x)
    (if (equal? x id)
        val
        (env x))))

(define empty-env (λ (id)
                    (error 'env "Unknown identifier ~s" id)))

(define (interp env mem0 expr)
  (match expr
    [(NumC n) (values (NumV n) mem0)]
    [(AddC a b)
     (let*-values ([(na mem1) (interp env mem0 a)]
                   [(nb mem2) (interp env mem1 b)])
       (values (NumV (+ (NumV-n na) (NumV-n nb))) mem2))]
    [(SeqC exprs)
     (let loop ([exprs exprs] [mem mem0])
       (if (empty? (rest exprs))
           (interp env mem (first expr))
           (loop (rest exprs)
                 (let-values ([(val mem1) (interp env mem (first exprs))])
                   mem1))))]
    [(MulC a b)
     (let*-values ([(na mem1) (interp env mem0 a)]
                   [(nb mem2) (interp env mem1 b)])
       (values (NumV (* (NumV-n na) (NumV-n nb))) mem2))]
    [(InvC x)
     (match-let-values ([((NumV n) mem1) (interp env mem0 x)])
       (if (not (= x 0))
           (values (NumV (/ 1 n)) mem1)
           (raise 'divide-by-zero-error)))]
    [(LtC a b)
     (match-let*-values ([((NumV na) mem1) (interp env mem0 a)]
                         [((NumV nb) mem2) (interp env mem1 b)])
       (values (BoolV (< na nb)) mem2))]
    [(EqC a b)
     (match-let*-values ([((NumV na) mem1) (interp env mem0 a)]
                         [((NumV nb) mem2) (interp env mem1 b)])
       (values (BoolV (= na nb)) mem2))]
    [(TrueC) (values (BoolV #t) mem0)]
    [(FalseC) (values (BoolV #f) mem0)]
    [(AndC a b)
     (match-let-values ([((BoolV b) mem1) (interp env mem0 a)])
       (if b
           (interp env mem1 b)
           (values (BoolV #f) mem1)))]
    [(OrC a b)
     (match-let-values ([((BoolV b) mem1) (interp env mem0 a)])
       (if (not b)
           (interp env mem1 b)
           (values (BoolV #t) mem1)))]
    [(NotC b)
     (match-let-values ([((BoolV vb) mem1) (interp env mem0 b)])
       (values (BoolV (not vb)) mem1))]
    [(IfC b t e)
     (match-let-values ([[(BoolV vb) mem1] (interp env mem0 b)])
       (interp env mem1 (if vb t e)))]
    
    [(IdC id)
     (values (lookup env id) mem0)]
    [(FunC argsym body)
     (values (FunV env argsym body) mem0)]
    [(ApplyC fexpr vexpr)
     (match-let*-values ([(fval mem1) (interp env mem0 fexpr)]
                         [(vval mem2) (interp env mem1 vexpr)])
       (match fval
         [(FunV fenv argsym body)
          (interp (extend fenv argsym vval) mem2 body)]
         [_ (error "Can't apply a non-function")]
         ))]

    [(BoxC expr)
     (let-values ([(val mem1) (interp env mem0 expr)])
       (store val mem1))]
    [(UnboxC bexpr)
     (match-let-values ([((BoxV addr) mem1) (interp env mem0 bexpr)])
       (values (lookup-address mem1 addr) mem1))]
    [(SetBoxC bexpr vexpr)
     (match-let*-values ([((BoxV addr) mem1) (interp env mem0 bexpr)]
                         [(val mem2) (interp env mem1 vexpr)])
       (values (BoxV addr) (passert mem2 addr boxvalkey val)))]
    ))

(define (store val mem)
  (let ([addr (make-addr)])
    (values (BoxV addr) (passert mem addr boxvalkey val))))

(define (lookup-address mem addr)
  (first (pfind mem addr boxvalkey #f Tri-obj)))

;                                                                 
;                                                                 
;                                                                 
;         ;                                                       
;         ;                                                       
;         ;                                                       
;     ;;; ;   ;;;;     ;;;;;   ;     ;    ;;; ;    ;;;;     ; ;;; 
;    ;   ;;   ;   ;   ;     ;  ;     ;   ;   ;;   ;    ;    ;;   ;
;   ;     ;  ;    ;;  ;        ;     ;  ;     ;        ;    ;     
;   ;     ;  ;;;;;;;  ;;;;     ;     ;  ;     ;   ;;;;;;    ;     
;   ;     ;  ;           ;;;;  ;     ;  ;     ;  ;;    ;    ;     
;   ;     ;  ;              ;  ;     ;  ;     ;  ;     ;    ;     
;    ;   ;;   ;       ;     ;  ;;   ;;   ;   ;;  ;    ;;    ;     
;     ;;; ;    ;;;;    ;;;;;    ;;;; ;    ;;; ;   ;;;; ;    ;     
;                                             ;                   
;                                        ;   ;;                   
;                                         ;;;;                    
;                                                                 

(define (desugar e)
  (match e
    [(SubS a b)
     (AddC (desugar a)
           (MulC (NumC -1) (desugar b)))]
    [(GtS a b) (LtC (desugar b) (desugar a))]
    [(AddC a b)
     (AddC (desugar a) (desugar b))]
    [(MulC a b)
     (MulC (desugar a) (desugar b))]
    [(NumC n)
     (NumC n)]
    [(InvC e)
     (InvC (desugar e))]
    [(LtC a b) (LtC (desugar a) (desugar b))]
    [(EqC a b) (EqC (desugar a) (desugar b))]
    [(AndC a b)
     (AndC (desugar a) (desugar b))]
    [(OrC a b)
     (OrC (desugar a) (desugar b))]
    [(NotC a)
     (NotC (desugar a))]     
    [(IfC b t e)
     (IfC (desugar b) (desugar t) (desugar e))]
    [(IdC id)
     (IdC id)]
    [(FunC argsym body)
     (FunC argsym (desugar body))]
    [(ApplyC fexpr vexpr)
     (ApplyC (desugar fexpr) (desugar vexpr))]
    [_ e]
    ))

; stdlib = (make-stdlib fns stdlib)
  
;(interp (desugar e))

; (AddC (NumC 3) (SubS (NumC 10) (NumC 4)))

;                                                        
;                                                        
;                                                        
;     ;;;;   ;                          ;                
;    ;    ;  ;                          ;                
;    ;       ;                          ;                
;   ;        ; ;;;;    ;;;;      ;;;    ;   ;;    ;;;;;  
;   ;        ;;   ;;   ;   ;    ;   ;   ;  ;;    ;     ; 
;   ;        ;     ;  ;    ;;  ;        ; ;;     ;       
;   ;        ;     ;  ;;;;;;;  ;        ;;;      ;;;;    
;   ;        ;     ;  ;        ;        ;  ;        ;;;; 
;    ;       ;     ;  ;        ;        ;   ;          ; 
;    ;    ;  ;     ;   ;        ;   ;   ;    ;   ;     ; 
;     ;;;;   ;     ;    ;;;;     ;;;    ;     ;   ;;;;;  
;                                                        
;                                                        
;                                                        
;                                                        

;(trace interp)
(interp empty-env '() (desugar (ApplyC
                                (FunC 'x
                                      (ApplyC
                                       (FunC 'f
                                             (ApplyC (IdC 'f)
                                                     (ApplyC (IdC 'f)
                                                             (IdC 'x))))
                                       (FunC 'x
                                             (MulC (IdC 'x)
                                                   (IdC 'x)))))
                                (NumC 3))))

(define (interpS env mem expr) (interp env mem (desugar expr)))

; The above code is expressed below in Racket.
#;((λ (x) ((λ (f) (f (f x)))
           (λ (x) (* x x))))
   3)

(interpS empty-env '()
         (ApplyC (FunC 'f
                       (ApplyC (IdC 'f) (NumC 10)))
                 (ApplyC (FunC 'x
                               (FunC 'y
                                     (AddC (IdC 'x) (IdC 'y))))
                         (NumC 3))))

#;(interpS empty-env (ApplyC
                      (FunC 'f
                            (ApplyC (FunC 'x
                                          (ApplyC (IdC 'f)
                                                  (MulC (IdC 'x) (NumC 3))))
                                    (NumC 100)))
                      (FunC 'y (MulC (IdC 'x) (IdC 'x)))))

#;((λ (f) ((λ (x) (f (* x 3))) 100))
   (λ (y) (* x x)))

(define (make-stdlib bindings stdlib)
  (if (empty? bindings)
      stdlib
      (extend (make-stdlib (rest bindings) stdlib)
              (first (first bindings))
              (let-values ([(v mem1) (interpS stdlib '() (second (first bindings)))])
                v))))

(define (make-stdlib2 bindings stdlib)
  (if (empty? bindings)
      stdlib
      (make-stdlib2 (rest bindings)
                    (extend stdlib
                            (first (first bindings))
                            (let-values ([(v mem1) (interpS stdlib '() (second (first bindings)))])
                              v)))))

(define (fixed-point f) (f (λ (x) ((fixed-point f) x))))

(define stdlib-defns
  (list (list 'even (FunC 'n
                          (IfC (EqC (IdC 'n) (NumC 0))
                               (TrueC)
                               (ApplyC (IdC 'odd) (SubS (IdC 'n) (NumC 1))))))
        (list 'odd (FunC 'n
                         (IfC (EqC (IdC 'n) (NumC 0))
                              (FalseC)
                              (ApplyC (IdC 'even) (SubS (IdC 'n) (NumC 1))))))
        (list 'fact (FunC 'n
                          (IfC (EqC (IdC 'n) (NumC 0))
                               (NumC 1)
                               (MulC (IdC 'n) (ApplyC (IdC 'fact)
                                                      (SubS (IdC 'n) (NumC 1)))))))))

(define stdlib (fixed-point (λ (s) (make-stdlib2 stdlib-defns s))))


(letrec ([even? (λ (n) (if (= n 0) #t (odd? (- n 1))))]
         [odd? (λ (n) (if (= n 0) #f (even? (- n 1))))])
  (even? 12))

#; (letrec ([even? (λ (n) (if (= n 0) #t (odd? (- n 1))))]
            [odd? (λ (n) (if (= n 0) #f (even? (- n 1))))])
     (even? 12))

(let ([b1 (box #f)]
      [b2 (box #f)])
  (let ([even? (λ args (apply (unbox b1) args))]
        [odd? (λ args (apply (unbox b2) args))])
    (set-box! b1 (λ (n) (if (= n 0) #t (odd? (- n 1)))))
    (set-box! b2 (λ (n) (if (= n 0) #f (even? (- n 1)))))
    (even? 12)))







