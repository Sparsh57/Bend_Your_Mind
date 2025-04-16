#lang racket

(require rackunit
         "asynclang.rkt"
         "lang.rkt"
         "event-loop.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 1: Await on a Promise that resolves immediately
;; Expected Behavior: The awaited Promise should unwrap to its resolved value (42).
;; Reasoning: AwaitC expects a PromiseV. Since PromiseC constructs a promise using
;; an executor that immediately calls resolve(42), AwaitC should extract 42.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (run
  (AwaitC
   (PromiseC
    (FunC '(resolve reject)
          (AppC (IdC 'resolve) (NumC 42))))))
 42)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 2: Await result of calling an async function
;; Expected Behavior: Should produce 5 (i.e., 4 + 1).
;; Reasoning: AsyncFunc wraps the function body in a promise implicitly.
;; AwaitC forces evaluation of the resulting promise. This mimics real async/await.
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
;; Test 3: AwaitC on a rejected Promise should not unwrap
;; Expected Behavior: Returns a PromiseV, not a plain number.
;; Reasoning: Since rejection logic is not modeled, the internal state remains
;; a PromiseV with no value unwrapped. AwaitC returns the original promise.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-pred
 PromiseV?
 (run
  (AwaitC
   (PromiseC
    (FunC '(resolve reject)
          (AppC (IdC 'reject) (NumC 999)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 4: Nested AwaitC inside an AsyncFunc
;; Expected Behavior: Resolves to 11 (10 + 1).
;; Reasoning: AsyncFunc body contains an AwaitC for a new Promise.
;; The outer AwaitC unwraps the returned promise from AsyncFunc. Nested await is honored. i.e.,The interpreter
;; respects the contract of nested async logic by fully evaluating the inner promise before returning the final value.
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
;; Test 5: AwaitC on non-promise input
;; Expected Behavior: Raises a runtime error.
;; Reasoning: AwaitC expects an expression that evaluates to a PromiseV.
;; Passing a raw NumC should cause a contract violation or fail with exn:fail.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-exn
 exn:fail?
 (λ ()
   (run (AwaitC (NumC 10)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 6: PromiseC with a non-function argument
;; Expected Behavior: Raises an error because the executor must be a function.
;; Reasoning: PromiseC expects a thunk of two arguments, resolve and reject.
;; A number is not a valid function and should fail with exn:fail.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-exn
 exn:fail?
 (λ ()
   (run (PromiseC (NumC 99)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 7: Async function returns a Promise directly
;; Expected Behavior: Should still unwrap and return 21 (20 + 1).
;; Reasoning: Even though AsyncFunc's body *is* a PromiseC, it still wraps it
;; in another Promise. AwaitC unwraps both layers, ultimately exposing the value.
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
;; Test 8: Deep async chain with multiple awaits and dependency passing
;; Expected Behavior: 7 + 3 + 1 = 11
;; Reasoning: Nested async/await calls must compose correctly. Each await
;; must wait for its inner result before resolving. This tests correctness
;; under multiple levels of dependency resolution.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (run
  (AwaitC
   (AppC
    (AsyncFunc '(a)
               (AwaitC
                (PromiseC
                 (FunC '(resolve reject)
                       (AppC (IdC 'resolve)
                             (AppC (IdC '+)
                                   (list
                                    (IdC 'a)
                                    (AppC (IdC '+) (list (NumC 1) (NumC 3))))))))))
    (list (NumC 7)))))
 11)

(displayln "✅ All async tests ran. If you see no failures, your async semantics work!")
