#lang racket

(provide make-slist-leaf-iterator subst-leftmost)

(define make-stack
  (lambda ()
    (let ([stk '()])
      (lambda (msg . args)
        (case msg
          [(empty?) (null? stk)]
          [(push) (set! stk (cons (car args) stk))]
          [(pop) (let ([top (car stk)])
                   (set! stk (cdr stk))
                   top)]
          [else
           (error 'stack "illegal message to stack object:" msg)])))))

(define make-slist-leaf-iterator
  (lambda (slist)
    (let ([stk (make-stack)])
      (stk 'push slist)
      (lambda (msg)
        (let loop ()
          (if (stk 'empty?)
              #f
              (let ([x (stk 'pop)])
                (cond
                  [(null? x)
                   (loop)]

                  [(null? (car x))
                   (stk 'push (cdr x))
                   (loop)]

                  [(pair? (car x))
                   (stk 'push (cdr x))
                   (stk 'push (car x))
                   (loop)]

                  [else
                   (stk 'push (cdr x))
                   (car x)]))))))))

(define subst-leftmost
  (lambda (a b c d)
    (nyi)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
