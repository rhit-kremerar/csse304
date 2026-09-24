#lang racket

(provide is-shadowed? convert-multip-calls convert-multip-lambdas convert-ifs)

(define list-recur
  (lambda (base-value list-proc)
    (letrec ([helper (lambda (ls)
                (if (null? ls)
                    base-value
                    (list-proc (car ls) (helper (cdr ls)))))])
      helper)))


(define list-recur-left
  (lambda (base-value list-proc)
    (letrec ([helper (lambda (ls answer)
                (if (null? ls)
                    answer
                    (helper (cdr ls) (list-proc answer (car ls)))))])
      (lambda (ls) (helper ls base-value)))))

(define is-shadowed?
  (lambda (var lc-exp)
    (letrec ([helper (lambda (exp bound?)
                (cond
                  [(symbol? exp) #f]
                  [(eq? (car exp) 'lambda)
                   (let ([param (car (cadr exp))])
                     (if (and bound? (eq? param var))
                         #t
                         (helper (caddr exp) (or bound? (eq? param var)))))]

                  [else (or (helper (car exp) bound?) (helper (cadr exp) bound?))]))])
      (helper lc-exp #f))))

(define convert-multip-calls
  (lambda (lcexp)
    (cond
      [(symbol? lcexp) lcexp]

      [(eq? (car lcexp) 'lambda)
       (list 'lambda (cadr lcexp) (convert-multip-calls (caddr lcexp)))]

      [else ((list-recur-left
         (convert-multip-calls (car lcexp))
         (lambda (answer next)
           (list answer (convert-multip-calls next))))
        (cdr lcexp))])))


(define convert-multip-lambdas
  (lambda (lcexp)
    (cond
      [(symbol? lcexp) lcexp]

      [(eq? (car lcexp) 'lambda)
       ((list-recur
         (convert-multip-lambdas (caddr lcexp))
         (lambda (param body)
           (list 'lambda
                 (list param)
                 body)))
        (cadr lcexp))]

      [else (list (convert-multip-lambdas (car lcexp)) (convert-multip-lambdas (cadr lcexp)))])))

(define convert-ifs
  (lambda (exp)
    (cond
      [(eq? exp #t) '(lambda (val1 val2) val1)]
      [(eq? exp #f) '(lambda (val1 val2) val2)]

      [(symbol? exp) exp]

      [(eq? (car exp) 'if)
       (list (convert-ifs (cadr exp))
             (convert-ifs (caddr exp))
             (convert-ifs (cadddr exp)))]

      [(eq? (car exp) 'lambda)
       (list 'lambda (cadr exp) (convert-ifs (caddr exp)))]

      [else (cons (convert-ifs (car exp)) 
             ((list-recur '() (lambda (current answer) (cons (convert-ifs current) answer))) (cdr exp)))])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
