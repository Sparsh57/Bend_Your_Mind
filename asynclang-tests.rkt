#lang racket

(require rackunit
         "asynclang.rkt"
         "lang.rkt"
         "event-loop.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 1: Await on a resolved Promise should produce the resolved value
;; Reasoning: AwaitC should unwrap the value a promise resolves to.
;; Expected: 42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (run
  (AwaitC
   (PromiseC
    (FunC '(resolve reject)
          (AppC (IdC 'resolve) (NumC 42))))))
 42)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 2: Async function should return a promise that resolves correctly
;; Reasoning: AsyncFunc wraps the body in a Promise. Awaiting it gives the result.
;; Expected: 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (run
  (AwaitC
   (AppC
    (AsyncFunc '(x)
               (AppC (IdC '+) (list (IdC 'x) (NumC 1))))
    (list (NumC 4)))))
 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 3: Promise that rejects should not be unwrapped by AwaitC
;; Reasoning: Since we don't implement reject-handling, AwaitC should still return a PromiseV
;; Expected: A PromiseV (not an unwrapped value)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-pred
 PromiseV?
 (run
  (AwaitC
   (PromiseC
    (FunC '(resolve reject)
          (AppC (IdC 'reject) (NumC 999)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 4: AsyncFunc body has inner AwaitC that waits for another Promise
;; Reasoning: Tests nested async/await flow
;; Expected: 11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (run
  (AwaitC
   (AppC
    (AsyncFunc '(x)
               (AwaitC
                (PromiseC
                 (FunC '(resolve reject)
                       (AppC (IdC 'resolve)
                             (AppC (IdC '+) (list (IdC 'x) (NumC 1))))))))
    (list (NumC 10)))))
 11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 5: AwaitC on a non-promise expression
;; Reasoning: Should raise an error, as AwaitC expects a PromiseV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-exn
 exn:fail?
 (λ ()
   (run (AwaitC (NumC 10)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 6: Promise executor is not a function
;; Reasoning: PromiseC expects a function of form (resolve, reject) => ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-exn
 exn:fail?
 (λ ()
   (run (PromiseC (NumC 99)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 7: Async function that returns a Promise directly
;; Reasoning: Even if the body returns a Promise, the AsyncFunc itself wraps it in another
;; Expected: 21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (run
  (AwaitC
   (AppC
    (AsyncFunc '(x)
               (PromiseC
                (FunC '(resolve reject)
                      (AppC (IdC 'resolve)
                            (AppC (IdC '+) (list (IdC 'x) (NumC 1)))))))
    (list (NumC 20)))))
 21)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DONE — all major behaviors covered
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(displayln "✅ All async tests ran. If you see no failures, your async semantics work!")
