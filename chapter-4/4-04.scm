; exercise 4.4

; use data-directed eval from exercise 4.3

(load "4-03.scm")

; install `and` and `or` special forms

(define (and-terms exp) (cdr exp))
(define (or-terms exp) (cdr exp))

(define (eval-and exp env)
  (eval-and-terms (and-terms exp) env))

(define (eval-or exp env)
  (eval-or-terms (or-terms exp) env))

(define (eval-and-terms terms env)
  (if (null? terms)
      'true
      (let ((first (car terms))
            (rest (cdr terms)))
        (if (true? (eval first env))
            (eval-and-terms rest env)
            'false))))


(define (eval-or-terms terms env)
  (if (null? terms)
      'false
      (let ((first (car terms))
            (rest (cdr terms)))
        (if (true? (eval first env))
            'true
            (eval-or-terms rest env)))))

(put 'eval 'and eval-and)
(put 'eval 'or eval-or)

; add in some more primitives for testing

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '= =)
        (list '< <)
        (list '> >)
        ))

(define the-global-environment (setup-environment))

; test

(eval '(define x 3) the-global-environment)
;Value: ok

(eval '(define y 5) the-global-environment)
;Value: ok

(eval '(and (< x 5) (> (+ y 1) 5)) the-global-environment)
;Value: true

(eval '(and (= (+ x 1) 3) (= y 5)) the-global-environment)
;Value: false

(eval '(or (= (+ x 1) 3) (= y 5)) the-global-environment)
;Value: true

(eval '(or (= y 0) (= x 0)) the-global-environment)
;Value: false