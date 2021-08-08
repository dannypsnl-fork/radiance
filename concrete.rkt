#lang typed/racket

(provide (all-defined-out))

(struct module
  ([filename : String]
   [inductive* : (Listof inductive)])
  #:transparent)

(struct inductive
  ([name : String])
  #:transparent)