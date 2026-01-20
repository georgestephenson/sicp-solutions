; exercise 4.13

; load evaluator code

(load "resources/ch4-mceval.scm")

; define make-unbound!

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((make-unbound? exp) (eval-make-unbound exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (make-unbound? exp)
  (tagged-list? exp 'make-unbound!))

(define (eval-make-unbound exp env)
  (remove-variable! (make-unbound-variable exp)
                    env)
  'ok)

(define (make-unbound-variable exp)
  (cadr exp))


; I believe make-unbound! should only remove the binding
; in the first frame, for the same reason define-variable!
; only adds a variable to the first frame.

; The main advantage is that this makes repeated calls to
; either procedure idempotent. For example, I can define
; the variable x one or many times, and remove it one or 
; many times, and I get the same result as if I defined
; and removed x once.

(define (remove-variable! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (error "Unbound variable: " var))
            ((eq? var (car vars))
             (remove-binding-from-frame! vars vals))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (remove-binding-from-frame! vars vals)
  (set-car! vars (cdr vars))
  (set-car! vals (cdr vals)))

; test

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '= =)
        ))

(define the-global-environment (setup-environment))

(eval '(define x 10) 
      the-global-environment)

(eval '(+ x 1)
      the-global-environment)
;Value: 11

(eval '(define (proc x)
         (+ x 2)) 
      the-global-environment)

(eval '(proc 0) 
      the-global-environment)
;Value: 2

(eval '(make-unbound! x) 
      the-global-environment)

(eval '(+ x 2)
      the-global-environment)
;Unbound variable x

(eval '(proc 0) 
      the-global-environment)
;Value: 2

(eval '(make-unbound! proc) 
      the-global-environment)

(eval '(proc 0) 
      the-global-environment)
;Unbound variable proc

(eval '(define x 33) 
      the-global-environment)

(eval '(+ x 33)
      the-global-environment)
;Value: 66