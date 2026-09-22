#lang racket

(provide slist-map slist-reverse slist-paren-count slist-depth slist-symbols-at-depth path-to make-c...r)

;s-list grammar
;<s-list>       ::= ( {<s-expression>}* )
;<s-expression> ::=  <symbol> | <s-list>

(define slist-map
  (lambda (a b)
    (cond
      [(null? b) '()]
      [(symbol? (car b)) (cons (a (car b)) (slist-map a (cdr b)))]
      [else (cons (slist-map a (car b)) (slist-map a (cdr b)))])))

(define slist-reverse
  (lambda (a)
    (cond
      [(null? a) '()]
      [(symbol? (car a)) (append (slist-reverse (cdr a)) (list (car a)))]
      [else (append (slist-reverse (cdr a)) (list (slist-reverse (car a))))])))

(define slist-paren-count
  (lambda (a)
    (cond
      [(null? a) 2]
      [(symbol? (car a)) (slist-paren-count (cdr a))]
      [else (+ (slist-paren-count (car a)) (slist-paren-count (cdr a)))])))

(define slist-depth
  (lambda (a)
    (cond
      [(null? a) 1]
      [(symbol? (car a)) (slist-depth (cdr a))]
      [else (max (+ 1 (slist-depth (car a))) (slist-depth (cdr a)))])))

(define slist-symbols-at-depth
  (lambda (a b)
    (cond
      [(null? a) '()]
      [(symbol? (car a))
       (if (= b 1) (cons (car a) (slist-symbols-at-depth (cdr a) b)) (slist-symbols-at-depth (cdr a) b))]
      [else (append (slist-symbols-at-depth (car a) (- b 1)) (slist-symbols-at-depth (cdr a) b))])))

(define path-to
  (lambda (a b)
    (cond
      [(null? a) #f]
      [(symbol? (car a))
       (if (eq? (car a) b)
           '(car)
           (if (path-to (cdr a) b)
               (cons 'cdr (path-to (cdr a) b))
               #f))]
      [else (if (path-to (car a) b)
           (cons 'car (path-to (car a) b))
           (if (path-to (cdr a) b)
               (cons 'cdr (path-to (cdr a) b))
               #f))])))

(define make-c...r
  (lambda (str)
    (apply compose
    (map (lambda (ch) (if (char=? ch #\a) car cdr)) (string->list str)))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
