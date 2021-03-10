#lang nanopass

(provide parse-core)

(define-language Core
  (terminals
   [symbol (id)]
   [number (num)])
  (Expr (e)
        id
        (cons e* ...)
        (lambda tele* ... e)
        (pi tele* ... e)
        (sigma tele* ... e)
        (app e e* ...)
        (type level))
  (Telescope (tele)
             (explicit id ty)
             (implicit id ty))
  (Type (ty) e)
  (Level (level) num))

(define-parser parse-core Core)

(module+ test
  (parse-core '(cons n m)))
