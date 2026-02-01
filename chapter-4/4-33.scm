; exercise 4.33

(load "resources/ch4-leval.scm")

(define primitive-procedures
  (list (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list 'map map)
        (list 'newline newline)
        (list 'display display)
        ))

; redefine text-of-quotation

(define (list->lambdas exp)
  (if (null? exp)
    '()
    (let ((first (list 'quote (car exp)))
          (rest (list->lambdas (cdr exp))))
      (make-lambda (list 'm) (list (list 'm first rest))))))

(define (text-of-quotation exp) 
  (let ((inner-exp (cadr exp)))
    (if (symbol? inner-exp)
        inner-exp
        (eval (list->lambdas inner-exp) the-global-environment))))

; test

(define the-global-environment (setup-environment))
(driver-loop)

; section 4.2.3 definitions

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;;; L-Eval input:
(car '(a b c))
;;; L-Eval value:
a

;;; L-Eval input:
(car (cdr '(a b c)))
;;; L-Eval value:
b

;;; L-Eval input:
(car (cdr (cdr '(a b c))))
;;; L-Eval value:
c