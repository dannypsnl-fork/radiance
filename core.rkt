#lang nanopass

(provide parse-core)

(define-language Core
  (terminals
   [symbol (id)]
   [number (num)])
  (Expr (e)
        id
        (cons e* ...)
        (lambda (tele* ...) e)
        (pi (tele* ...) e)
        (sigma (tele* ...) e)
        (app e e* ...)
        (type level))
  (Telescope (tele)
             (explicit id ty)
             (implicit id ty))
  (Type (ty) e)
  (Level (level) num))

(define-pass subst : Core (e m) -> Core ()
  (S : Expr (e) -> Expr ()
     [,id (if (hash-ref m id #f)
              (hash-ref m id)
              id)]
     [(cons ,[e*] ...)
      `(cons ,e* ...)]
     [(lambda (,tele* ...) ,e)
      (define m2 (hash-copy m))
      (for ([tele tele*])
        (nanopass-case
         (Core Telescope) tele
         [(explicit ,id ,ty)
          (hash-remove! m2 id)]
         [(implicit ,id ,ty)
          (hash-remove! m2 id)]))
      `(lambda (,tele* ...) ,[subst e m2])]
     [(pi (,tele* ...) ,[e])
      `(pi (,tele* ...) ,e)]
     [(sigma (,tele* ...) ,[e])
      `(sigma (,tele* ...) ,e)]
     [(app ,[e] ,[e*] ...)
      `(app ,e ,e* ...)])
  (S e))

(define-pass β-reduce : Core (e) -> Core ()
  (R : Expr (e) -> Expr ()
     [(app (lambda (,tele* ...) ,e) ,e* ...)
      (let ([tele*-len (length tele*)]
            [e*-len (length e*)])
        (unless (= tele*-len e*-len)
          (error 'arity "need ~a, get ~a" tele*-len e*-len)))
      (define m (make-hash))
      (for ([tele tele*]
            [e e*])
        (nanopass-case
         (Core Telescope) tele
         [(explicit ,id ,ty)
          (hash-set! m id e)]
         [(implicit ,id ,ty)
          (hash-set! m id e)]))
      (subst e m)])
  (R e))

(define-parser parse-core Core)

(module+ test
  (require rackunit)

  (check-equal?
   (unparse-Core
    (β-reduce (parse-core
               '(app (lambda ([explicit x A] [explicit y A])
                       (cons x y))
                     a b))))
   '(cons a b))

  ; should escape
  (check-equal?
   (unparse-Core
    (β-reduce (parse-core
               '(app (lambda ([explicit x A])
                       (lambda ([explicit x A])
                         x))
                     a))))
   '(lambda ([explicit x A]) x)))
