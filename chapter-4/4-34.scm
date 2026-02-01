; exercise 4.34

(load "resources/ch4-leval.scm")

; implementing the cons, car and cdr as special forms
; based on the lazy definitions in section 4.2.3

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((cons? exp)
          (list 'cons
                (make-procedure (list 'm)
                                (list (list 'm (car-cons exp) (cdr-cons exp)))
                                env)))
        ((car? exp)
          (eval (list (cadr (eval (cadr exp) env))
                      (list 'lambda (list 'p 'q) 'p))
                env))
        ((cdr? exp)
          (eval (list (cadr (eval (cadr exp) env))
                      (list 'lambda (list 'p 'q) 'q))
                env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)             ; clause from book
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (cons? z)
  (tagged-list? z 'cons))

(define (car? z)
  (tagged-list? z 'car))

(define (cdr? z)
  (tagged-list? z 'cdr))

(define (car-cons z)
  (cadr z))

(define (cdr-cons z)
  (caddr z))

(define primitive-procedures
  (list (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list 'map map)
        (list 'newline newline)
        (list 'display display)))

(define (cons-tagged? object)
  (and (pair? object)
       (eq? (car object) 'cons)))

; Disclaimer: claude helped to write the display logic here.
; Building and traversing cons structures is extremely tedious, and not very interesting.

(define print-limit 10)

(define (user-print object)
  (if (cons-tagged? object)
      (begin
        (display "(")
        (user-print-list object 0)
        (display ")"))
      (user-print-atom object)))

(define (user-print-list object count)
  (cond ((>= count print-limit)
         (display " ..."))
        ((cons-tagged? object)
         (if (> count 0) (display " "))
         (user-print-atom (force-car object))
         (user-print-list (force-cdr object) (+ count 1)))
        ((null? object) 'done)
        (else
         (display " . ")
         (user-print-atom object))))

(define (user-print-atom object)
  (cond ((cons-tagged? object)
         (display "(")
         (user-print-list object 0)
         (display ")"))
        ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        (else (display object))))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)
      (newline)))
  (driver-loop))

(define (force-car tagged-cons)
  (let ((thunk (cadr tagged-cons)))
    (actual-value '(thunk (lambda (p q) p))
                  (extend-environment (list 'thunk) (list thunk) the-global-environment))))

(define (force-cdr tagged-cons)
  (let ((thunk (cadr tagged-cons)))
    (actual-value '(thunk (lambda (p q) q))
                  (extend-environment (list 'thunk) (list thunk) the-global-environment))))

; test

(define the-global-environment (setup-environment))
(driver-loop)

;;; L-Eval input:
(cons 'hello 'world)
;;; L-Eval value:
;(hello . world)

;;; L-Eval input:
(cons 1 (cons 2 (cons 3 (cons 4 '()))))
;;; L-Eval value:
;(1 2 3 4)

(define ones (cons 1 ones))

;;; L-Eval input:
ones
;;; L-Eval value:
;(1 1 1 1 1 1 1 1 1 1 ...)