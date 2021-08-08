#lang racket

(require "parser.rkt")

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "radiance"
   #:args args
   (handle args)))

(define (handle args)
  (match args
    [(list "run" filename)
     (run-singlefile filename)]
    [(list "ast" filename)
     (define mod (parsing filename))
     (displayln mod)]
    [bad (printf "bad args ~a" bad)]))

(define (parsing filename)
  (define file (open-input-file filename))
  (parse filename file))
(define (run-singlefile filename)
  (define mod (parsing filename))
  (eval-module mod))

(define (eval-module mod)
  (void))
