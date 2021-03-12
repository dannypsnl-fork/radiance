#lang typed/racket

(provide Type
         Term
         Lambda Pi
         FreeVar
         Explicit Implicit
         (struct-out Telescope))

(define-type Type Term)
(define-type Term (U Symbol
                     Lambda Pi
                     FreeVar))

(struct FreeVar
  ([val : Symbol]
   [typ : Type])
  #:transparent)
; (λ ({A : Type} [x : A] [y : A])
;   (tuple x y))
(struct Lambda
  ([tele+ : (Listof Telescope)]
   [body : Term])
  #:transparent)
(struct Pi
  ([tele+ : (Listof Telescope)]
   [body : Term])
  #:transparent)
(struct Telescope
  ([var : Symbol]
   [typ : Type])
  #:transparent)
(struct Explicit Telescope () #:transparent)
(struct Implicit Telescope () #:transparent)
