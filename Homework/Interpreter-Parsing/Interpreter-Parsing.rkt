#lang racket

(require "../chez-init.rkt")
(provide parse-exp unparse-exp)

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (data number?)]
  [lambda-exp
   (ids (lambda (x) (or (symbol? x) (and (list? x) (andmap symbol? x)))))
   (bodies (lambda (x) (and (list? x) (andmap expression? x))))]
  [app-exp
   (proc expression?)
   (args (lambda (x) (and (list? x) (andmap expression? x))))]
  [let-exp
   (type symbol?)
   ;doesn't have to be named
   (name (lambda (x) (or (symbol? x) (not x))))
   (ids (lambda (x) (and (list? x) (andmap symbol? x))))
   (vals (lambda (x) (and (list? x) (andmap expression? x))))
   (bodies (lambda (x) (and (list? x) (andmap expression? x))))]
  [if-exp
   (test expression?)
   (then expression?)
   ;doesn't have to have an else statement
   (else (lambda (x) (or (expression? x) (not x))))]
  [set-exp
   (id symbol?)
   (value expression?)])

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

;for lets
(define parse-bindings
  (lambda (bindings)
    (list (map car bindings) (map (lambda (binding) (parse-exp (2nd binding))) bindings))))

;for these it's really just making sure you are filling in the values in the right position for a given datatype
(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(number? datum) (lit-exp datum)]
      [(pair? datum)
       
       ;each of these handles a different datatypes
       (cond

         ;easiest is just a lambda
         [(eqv? (car datum) 'lambda) (lambda-exp (2nd datum) (map parse-exp (cddr datum)))]

         ;yay lets
         [(or (eqv? (car datum) 'let) (eqv? (car datum) 'let*) (eqv? (car datum) 'letrec))

          
          (cond
            ;named let
            [(and (eqv? (car datum) 'let)
                  (symbol? (2nd datum)))
             (let ([bindings (parse-bindings (3rd datum))])
               (let-exp 'let
                        (2nd datum)
                        (car bindings)
                        (2nd bindings)
                        (map parse-exp (cdddr datum))))]
            ;other lets
            [else (let ([bindings (parse-bindings (2nd datum))])
               (let-exp (car datum)
                        #f
                        (car bindings)
                        (2nd bindings)
                        (map parse-exp (cddr datum))))])]

         ;needs to handle with or without an else statement
         [(eqv? (car datum) 'if)
          (if (= (length datum) 3)
              (if-exp (parse-exp (2nd datum))
                      (parse-exp (3rd datum))
                      #f)
              (if-exp (parse-exp (2nd datum))
                      (parse-exp (3rd datum))
                      (parse-exp (4th datum))))]
         
         [(eqv? (car datum) 'set!) (set-exp (2nd datum) (parse-exp (3rd datum)))]

         ;only other option i put at end because it makes me not have to think about the check
         [else (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))])]

      [else (error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (exp)
    (cases expression exp

      ;each of these handles one datatype, similar to parse
      [var-exp (id) id]
      [lit-exp (data) data]
      [lambda-exp (ids bodies) (cons 'lambda (cons ids (map unparse-exp bodies)))]
      [app-exp (proc args) (cons (unparse-exp proc) (map unparse-exp args))]
      
      ;handle the different lets separately
      [let-exp (type name ids vals bodies)
       (let ([bindings
              (map (lambda (id val) (list id (unparse-exp val)))
                   ;map can have multiple args which is cool and helpful here
                   ids vals)])
         (if name
             (cons type (cons name (cons bindings (map unparse-exp bodies))))
             (cons type (cons bindings (map unparse-exp bodies)))))]

      ;using same names here for sanity
      [if-exp (test then else)
       (if else
           ;this is so it can do ifs with or without elses
           (list 'if
                 (unparse-exp test)
                 (unparse-exp then)
                 (unparse-exp else))
           (list 'if
                 (unparse-exp test)
                 (unparse-exp then)))]
      
      ;set is pretty typical
      [set-exp (id value) (list 'set! id (unparse-exp value))])))

; An auxiliary procedure that could be helpful.
(define var-exp?
  (lambda (x)
    (cases expression x
      [var-exp (id) #t]
      [else #f])))

;----------------------- QUICK TESTS ----------------------------

(unparse-exp (parse-exp '(lambda (x) 1 x (+ x 1))))
(unparse-exp (parse-exp '(lambda x x y)))
(unparse-exp (parse-exp '(let ((x 1) (y 2)) x y (+ x y))))
(unparse-exp (parse-exp '(let* ((x 1) (y x)) x y (+ x y))))
(unparse-exp (parse-exp '(letrec ((f (lambda (x) (f x)))) f (f 1))))
(unparse-exp (parse-exp '(let loop ((x 1)) x (loop x))))
(unparse-exp (parse-exp '(if x 1 2)))
(unparse-exp (parse-exp '(if x 1)))
(unparse-exp (parse-exp '(set! x 10)))
(unparse-exp (parse-exp '(lambda (x y) (if x (set! y 10) (set! y 20))y)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))