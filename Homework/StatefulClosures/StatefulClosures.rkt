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
                  ;not a leaf
                  [(null? x)
                   (loop)]
                  ;not a leaf but could be one later
                  [(null? (car x))
                   (stk 'push (cdr x))
                   (loop)]
                  ;x is another list. search x then rest LIFO
                  [(pair? (car x))
                   (stk 'push (cdr x))
                   (stk 'push (car x))
                   (loop)]
                  ;x is a leaf so return. the iterators 'next call is done
                  ;add rest to stack for when iterator is used again
                  [else
                   (stk 'push (cdr x))
                   (car x)]))))))))

;a = new
;b = old
;c = slist
;d = func
(define subst-leftmost
  (lambda (a b c d)
    (letrec ([helper (lambda (slist)
                (cond
                  ;adding boolean to return list so it knows if it can stop
                  [(null? slist) (cons '() #f)]
                  [(pair? (car slist))
                   (let ([car-result (helper (car slist))])
                     (if (cdr car-result)
                         ;fiund dont search rest
                         (cons (cons (car car-result) (cdr slist)) #t)
                         ;search rest
                         (let ([cdr-result (helper (cdr slist))])
                           (cons (cons (car car-result) (car cdr-result))
                           (cdr cdr-result)))))]
                  ;make replacement
                  [(d (car slist) b) (cons (cons a (cdr slist))  #t)]
                  [else
                   (let ([cdr-result (helper (cdr slist))])
                     (cons (cons (car slist) (car cdr-result)) (cdr cdr-result)))]))])
      (car (helper c)))))


;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
