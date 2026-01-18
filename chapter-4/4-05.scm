; exercise 4.5

; load evaluator code

(load "resources/ch4-mceval.scm")

; leave the driver-loop commented out

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'assoc assoc)
        (list 'cadr cadr)
        (list 'display display)
        ))

(define the-global-environment (setup-environment))

; modify the handling of cond to support the extended syntax
; (<test> => <recipient>)

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (let ((predicate (cond-predicate first))
                  (actions (cond-actions first)))
              (if (eq? (car actions) '=>)
                  (list
                    (list 'lambda (list 'value)
                          (make-if 'value
                                    (list (cadr actions) 'value)
                                    (expand-clauses rest)))
                    predicate)
                  (make-if predicate
                          (sequence->exp actions)
                          (expand-clauses rest))))))))

; test

(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false))
;Value: 2

(eval '(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false)) 
      the-global-environment)
;Value: 2
