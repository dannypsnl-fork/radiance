#lang nanopass

(define-language Core
  (terminals
   [symbol (id)]
   [number (num)])
  (Expr (e)
        id
        (record e1 e2)
        (lambda (id ty) ty2 e)
        (pi (id ty) e)
        (sigma (id ty) e)
        (app e1 e2)
        (type level))
  (Type (ty) e)
  (Level (level) num))

(define-language Concrete
  (entry Stmt)
  (extends Core)
  (Stmt (s)
        (+ (define id ty e))))

(define-pass concrete->core : (Concrete Expr) (e) -> (Core Expr) ()
  (R : Expr (e) -> Expr ())
  (R e))

(define type-env (make-hash))
(hash-set! type-env 'Type 'Type1)
(hash-set! type-env 'Nat 'Type)
(define value-env (make-hash))

(define-pass <-type : Core (e) -> Core ()
  (I : Expr (e) -> Expr ()
     [,id (hash-ref type-env id)]
     [(lambda (,id ,ty) ,ty2 ,[e])
      `(pi (,id ,ty) ,ty2)]
     [(record ,e1 ,e2)
      `(sigma (,(gensym) ,e1) ,e2)]
     [(app ,[e1] ,[e2])
      (nanopass-case (Core Type) e1
                     [(pi (,id ,ty) ,e)
                      (unless (equal? ty e2)
                        (error 'type-mismatched
                               "~a <=> ~a"
                               ty e2))
                      e]
                     [else (error 'non-appliable)])])
  (I e))

(define-pass ty-check : (Concrete Stmt) (s) -> * ()
  (C : Stmt (s) -> * ()
     [(define ,id ,ty ,e)
      (hash-set! type-env id (concrete->core ty))
      (unless (equal? (concrete->core ty) (<-type (concrete->core e)))
        (error 'type-mismatched
               "~a <=> ~a"
               ty (<-type (concrete->core e))))
      (hash-set! value-env id (concrete->core e))
      (void)])
  (C s))

(define-parser parse Concrete)

(ty-check (parse '(define zero Nat zero)))
(ty-check (parse '(define suc (pi (x Nat) Nat)
                    (lambda (x Nat) Nat (record suc x)))))
(ty-check (parse '(define three Nat (app suc (app suc zero)))))

(displayln type-env)
(displayln value-env)
