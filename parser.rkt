#lang racket

(provide parse)

(require "lexer.rkt"
         "concrete.rkt")

(define current-parser (make-parameter #f))

; ast
(struct expr () #:transparent)
(struct binary expr (op left right) #:transparent)

; parse
(define (parse filename input)
  (parameterize ([current-parser (make-parser filename input)])
    (consume 'module)
    (define mod-name (token-val (consume 'identifier)))
    (let loop ([inductive* '()])
      (cond
        [(predict? 'inductive)
         (define ind (parse-inductive))
         (loop (append inductive* (list ind)))]
        [else (module mod-name inductive*)]))))

(define (parse-inductive)
  (consume 'inductive)
  (define name (token-val (consume 'identifier)))
  (consume ':)
  (define typ (parse-type))
  (let loop ([constructor* '()])
    (cond
      [(predict? 'identifier)
       (define c (parse-constructor))
       (loop (cons c constructor*))]
      [else (inductive name constructor*)])))

(define (parse-constructor)
  (define name (token-val (consume 'identifier)))
  (consume ':)
  (define typ (parse-type))
  (constructor name typ))

(define (parse-type)
  (define t (token-val (consume 'identifier)))
  (if (predict? '->)
      (begin (consume '->)
             (pi (list (telescope (gensym 'param) t))
                 (parse-type)))
      t))

; helper
(struct parser (name lexer tokens offset)
  #:mutable
  #:transparent)

(define (make-parser name input)
  (define lexer (lex name input))
  (parser name lexer (stream) 0))

(define (peek [n 0])
  (get-token (+ (parser-offset (current-parser)) n)))
(define (put-back [n 1])
  (set-parser-offset! (current-parser) (- (parser-offset (current-parser)) n)))
(define (take [n 1])
  (define origin (parser-offset (current-parser)))
  (set-parser-offset! (current-parser) (+ origin n))
  (get-token origin))
(define (consume #:put-back [n 0] . wants)
  (with-handlers ([(lambda (e) #t)
                   (lambda (e)
                     (put-back n)
                     (raise e))])
    (apply predict wants))
  (take (length wants)))
(define (predict . wants)
  (for ([i (length wants)]
        [want wants])
    (define tok (peek i))
    (unless (eq? (token-typ tok) want)
      (error 'unexpected-token "want ~a, got ~a" want tok))))
(define (predict? . wants)
  (let/cc return
    (with-handlers ([(λ (e) #t)
                     (λ (e) (return #f))])
      (apply predict wants))
    #t))

(define (get-token fixed-offset)
  (define p (current-parser))
  (when (stream-empty? (parser-tokens p))
    (increase-token-stream p))
  (define tokens (parser-tokens p))
  (if (>= fixed-offset (stream-length tokens))
      (let ([last-token (stream-ref tokens (sub1 (stream-length tokens)))])
        (case (token-typ last-token)
          [(EOF) last-token]
          [else (increase-token-stream p)
                (get-token fixed-offset)]))
      (stream-ref tokens fixed-offset)))
(define (increase-token-stream p)
  (define l (parser-lexer p))
  (define new-last-token (channel-get (lexer-tokens l)))
  (set-parser-tokens! p
                      (stream-append (parser-tokens p) (stream new-last-token))))



(module+ test
  (require rackunit)

  (define (parse-ind name input)
    (parameterize ([current-parser (make-parser name input)])
      (parse-inductive)))

  (check-equal? (parse-ind "parsing" (open-input-string "
  inductive Nat : Type
    z : Nat
    s : Nat -> Nat
  "))
                (inductive "Nat"))

  (test-case "increase token stream automatically"
             (define (test-pos l c)
               (pos "" l c))
             (define lexer (lex "" (open-input-string "
             inductive Nat : Type
               z : Nat
               s : Nat -> Nat
             ")))
             (parameterize ([current-parser (parser "" lexer (stream) 0)])
               (check-equal? (get-token 4)
                             (token 'identifier "z" (test-pos 2 16) (test-pos 2 17))))))
