; exercise 4.10

; load evaluator code

(load "resources/ch4-mceval.scm")

; leave the driver-loop commented out

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '= =)
        ))

(define the-global-environment (setup-environment))

; a simple example of designing and implementing new syntax
; would be if we have to use keywords "then" and "else" 
; within an "if" statement

(define (if? exp)
  (and (tagged-list? exp 'if)
       (>= (length exp) 4)
       (eq? (caddr exp) 'then)))

(define (if-consequent exp) (cadddr exp))

(define (if-alternative exp)
  (let ((rest (cddddr exp)))
    (if (and (pair? rest) (eq? (car rest) 'else))
        (cadr rest)
        'false)))

(eval '(if (= 0 0) 
        then "right result -- then" 
        else "wrong result -- else") 
      the-global-environment)
;Value: "right result -- then"

(eval '(if (= 1 2) 
        then "wrong result -- then" 
        else "right result -- else") 
      the-global-environment)
;Value: "right result -- else"

(eval '(if (= 5 5) 
        then "right result -- then") 
      the-global-environment)
;Value: "right result -- then"