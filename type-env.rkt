#lang typed/racket

(require "term.rkt")

(struct TypeEnv
  ([parent : (Option TypeEnv)]
   [binds : (Immutable-HashTable Symbol Type)])
  #:transparent)

(: make-TypeEnv (->* [(Listof (Pair Symbol Type))]
                     [TypeEnv]
                     ;--------
                     TypeEnv))
(define (make-TypeEnv l [parent #f])
  (TypeEnv parent (make-immutable-hash l)))

(: typeof : TypeEnv Term -> Type)
(define (typeof tyenv term)
  (match term
    [(Lambda tele+ body)
     (define param-typs
       (map (λ ([tele : Telescope])
              (cons (Telescope-var tele) (Telescope-typ tele)))
            tele+))
     (define new-env (make-TypeEnv param-typs tyenv))
     (Pi tele+ (typeof new-env body))]
    [id #:when (symbol? term)
        (define r? (hash-ref (TypeEnv-binds tyenv) id
                             (λ ()
                               (if (TypeEnv-parent tyenv)
                                   (typeof (TypeEnv-parent tyenv) id)
                                   #f))))
        (unless r?
          (error 'not-found "~a" id))
        r?]))

(module+ test
  (require typed/rackunit)

  (define env (make-TypeEnv '((zero . Nat))))
  (check-equal? (typeof env 'zero)
                'Nat)

  (check-equal? (typeof env (Lambda (list (Explicit 'x 'Nat))
                                    'x))
                (Pi (list (Explicit 'x 'Nat)) 'Nat)))
