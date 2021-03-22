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
(define value-env (make-hash))

(define (type-> exp act)
  (unless (equal? exp act)
    (error 'type-mismatched
           "~a <=> ~a"
           exp act)))

(define-pass subst : Core (e id2) -> Core ()
  (S : Expr (e) -> Expr ()
     [,id (if (equal? id id2)
              id2
              id)]
     [(lambda (,id ,ty) ,ty2 ,e)
      (if (equal? id id2)
          `(lambda (,id ,ty) ,ty2 ,e)
          `(lambda (,id ,ty) ,ty2 ,(subst e id2)))])
  (S e))

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
                      (type-> ty e2)
                      (subst e id)]
                     [else (error 'non-appliable)])])
  (I e))

(define-pass ty-check : (Concrete Stmt) (s) -> * ()
  (C : Stmt (s) -> * ()
     [(define ,id ,ty ,e)
      (hash-set! type-env id (concrete->core ty))
      (type-> (concrete->core ty) (<-type (concrete->core e)))
      (hash-set! value-env id (concrete->core e))
      (void)])
  (C s))

(define-parser parse Concrete)

(hash-set! type-env 'Nat 'Type)
(hash-set! type-env 'Vec '(pi (a Type)
                              (pi (len Nat)
                                  Type)))

(ty-check (parse '(define zero Nat zero)))
(ty-check (parse '(define suc (pi (x Nat) Nat)
                    (lambda (x Nat) Nat (record suc x)))))

(ty-check (parse '(define nil (sigma [T Type] (app (app Vec T) zero)) nil)))
#;(ty-check (parse '(define :: (sigma (T Type) (sigma (n Nat)
                                                    (pi [v (app (app Vec T) n)]
                                                        (pi [e T] (app (app Vec T) (app suc n))))))
                    (sigma (T Type)
                           (sigma (n Nat)
                                  (lambda (v (app (app Vec T) n))
                                    (lambda (e T)
                                      (record (record :: v) e))))))))

(ty-check (parse '(define three Nat (app suc (app suc (app suc zero))))))

(hash-ref type-env 'nil)
(hash-ref value-env 'nil)
