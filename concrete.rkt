#lang typed/racket

(provide (all-defined-out))

(struct module
  ([filename : String]
   [inductive* : (Listof inductive)])
  #:transparent)

(struct inductive
  ([name : String]
   [constructor* : (Listof constructor)])
  #:transparent)

(struct constructor
  ([name : String]
   [typ : type])
  #:transparent)

(define-type type (U sigma pi String))
(struct sigma
  ([telescope* : (Listof telescope)]
   [ret : type])
  #:transparent)
(struct pi
  ([telescope* : (Listof telescope)]
   [ret : type]) #:transparent)

(struct telescope
  ([name  : Symbol]
   [typ : type])
  #:transparent)
