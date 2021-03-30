#lang racket

(require syntax/parse/define)

(begin-for-syntax
  (define-syntax-class bind
    (pattern (x:id : typ)))
  (define-syntax-class cons
    (pattern (x:id : typ)
             #:attr def #'(define x (make-the typ 'x)))
    (pattern (x:id telescope:bind ... : typ)
             #:attr args #'(list telescope.x ...)
             #:attr checking #'(list telescope.typ ...)
             #:attr def #'(define (x telescope.x ...)
                            (for ([expect checking]
                                  [actual args])
                              (unless (equal? (the-typ actual) expect)
                                (error 'type-mismatched
                                       "expect: ~a, get: ~a" expect (the-typ actual))))
                            (make-the typ (list 'x telescope.x ...))))))

(struct the
  (base typ val)
  #:methods gen:custom-write
  [(define (write-proc t port mode)
     (fprintf port "~a" (the-val t)))]
  #:transparent)
(define (make-the typ val)
  (the #f typ val))

(define Type (make-the 'Type 'Type))

(define-syntax-parser data
  [(_ x:id c*:cons ...)
   #'(begin
       (define x (make-the Type 'x))
       c*.def
       ...)]
  [(_ (x:id b*:bind ...) c*:cons ...)
   #'(begin
       (define x (make-the Type 'x))
       c*.def
       ...)])

(data Bool
      [true : Bool]
      [false : Bool])
(data Nat
      [zero : Nat]
      [suc (x : Nat) : Nat])
;(data (Vec [T : Type] [n : Nat])
;      [nil : (Vec T zero)]
;      [cons (n : Nat) (x : T) (v : (Vec T n)) : (Vec T (suc n))])

;(suc false)
