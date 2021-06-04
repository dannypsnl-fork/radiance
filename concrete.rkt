#lang nanopass

(define-language Concrete
  (entry Stmt)
  (terminals
   [symbol (id)]
   [number (num)])
  (Stmt (s)
        (data id ctor* ...))
  (Ctor (ctor)
        (id ty))
  (Expr (e)
        id
        (λ (tele* ...) e)
        (-> ty* ... ty)
        (Π (tele* ...) e)
        (record e* ...)
        (Σ (tele* ...) e)
        (app e e* ...)
        (type level))
  (Telescope (tele)
             (id ty))
  (Type (ty) e)
  (Level (level) num))

(define-parser parse-concrete Concrete)

(parse-concrete
 '(data Nat
        [z Nat]
        [s (-> Nat Nat)]))
