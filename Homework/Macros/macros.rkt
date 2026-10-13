
#lang racket

(require (for-syntax racket/list))

(provide init all-equal begin-unless range-cases for)

(define-syntax (init stx)
  (syntax-case stx ()
    [(_ x ...)
     #'(begin (define x 0) ...)]))

(define-syntax (all-equal stx)
  (syntax-case stx ()
    [(_ a b)
     #'(equal? a b)]
    [(_ a b rest ...)
     #'(let ([x a] [y b])
         (and (equal? x y)
              (all-equal y rest ...)))]))

(define-syntax (begin-unless stx)
  (syntax-case stx ()
    [(_ var)
     #'var]
    [(_ var body rest ...)
     #'(if var
           var
           (begin
             body
             (begin-unless var rest ...)))]))

(define-syntax (range-cases stx)
  (syntax-case stx (else <)
    [(_ val (else body))
     #'body]
    [(_ val (< cutoff body) rest ...)
     #'(let ([x val])
         (if (< x cutoff)
             body
             (range-cases x rest ...)))]))

(define-syntax (for stx)
  (syntax-case stx (:)
    [(_ (init : test : update ...) body ...)
     #'(begin
         init
         (let loop ()
           (when test
             body ...
             update ...
             (loop))))]))
