#lang racket

(provide (struct-out Tri))
(provide make-plist plist-sat? plist-assert plist-find)

; A "plist" or "property list" is a list of triples
; which are usually called "subject", "predicate" and "object".
; Think of it as a table with three columns with those names.
; Such a plist is a generally useful data structure and this
; file provides a basic (i.e. inefficient) implementation of
; them for illustration. By adding indexes for the subject and
; predicate using hash tables, it is possible to make
; operations on plists very efficient, but we leave that as
; an exercise and illustrate the idea here.
(struct Tri (sub pred obj) #:transparent #:mutable)

(define (make-plist) '())

; Checks whether the given subject, predicate and object
; are present in the plist. Any of the three arguments can
; be given #f in which case it won't be checked for.
;
; When searching a plist, the subject and object are usually
; checked using eq? (referential equality) whereas the predicate
; is checked using equal? (value equality).
(define (plist-sat? plist sub pred obj)
  (let loop ([plist plist])
    (if (empty? plist)
        #f
        (let ([t (first plist)])
          (or (and (or (not sub) (eq? sub (Tri-sub t)))
                   (or (not pred) (equal? pred (Tri-pred t)))
                   (or (not obj) (eq? obj (Tri-obj t))))
              (loop (rest plist)))))))

; Augments the plist if necessary with an association between
; the given triple. What plist-assert ensures is that
; if you test with a plist-sat? on the same sub/pred/obj
; combination after a plist-assert, plist-sat? will return #t.
(define (plist-assert plist sub pred obj)
  (or (and (plist-sat? plist sub pred obj) plist)
      (cons (Tri sub pred obj) plist)))

; Returns a list of triples which match the given
; subject, predicate and object. As with plist-sat?
; any of these can be given #f. The `extract` argument,
; when given, needs to be a function that takes a triple
; and produces some value. Usually you use one of
; Tri-sub, Tri-pred or Tri-val for this to extract the
; corresponding field.
(define (plist-find plist sub pred obj [extract #f])
  (let ([results
         (filter (λ (t)
                 (and (or (not sub) (eq? sub (Tri-sub t)))
                      (or (not pred) (equal? pred (Tri-pred t)))
                      (or (not obj) (eq? obj (Tri-obj t)))))
               plist)])
    (if (empty? results)
        #f
        (if extract
            (map extract results)
            results))))


  