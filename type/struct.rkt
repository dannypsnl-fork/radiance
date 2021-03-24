#lang racket

(struct Telescope
  (name ty)
  #:transparent)
(struct Pi
  (base
   tele*
   exp)
  #:property prop:procedure
  (struct-field-index base)
  #:transparent)

(define (typeof e)
  e)

(define (make-Pi tele* exp)
  (Pi (λ arg*
        (unless (= (length tele*) (length arg*))
          (error 'arity))
        (for ([tele tele*]
              [arg arg*])
          (unless (equal? (typeof arg) (Telescope-ty tele))
            (error 'type-mismatched))))
      tele* exp))

(define suc (make-Pi (list (Telescope 'x 'Nat)) '(suc x)))
(suc 'Nat)
