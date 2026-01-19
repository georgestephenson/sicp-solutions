; exercise 4.8

; load solution to exercise 4.6

(load "4-06.scm")

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '= =)
        ))

(define the-global-environment (setup-environment))

; redefine let->combination

(define (let-vars bindings)
  (map car bindings))

(define (let-exps bindings)
  (map cadr bindings))

(define (let->combination exp)
  (if (variable? (cadr exp))
      (named-let->combination exp)
      (orig-let->combination exp)))

(define (orig-let->combination exp)
  (let* ((bindings (cadr exp))
         (vars (let-vars bindings))
         (exps (let-exps bindings))
         (body (cddr exp)))
    (cons (cons 'lambda (cons vars body)) exps)))

(define (named-let->combination exp)
  (let* ((proc (cadr exp))
         (bindings (caddr exp))
         (vars (let-vars bindings))
         (exps (let-exps bindings))
         (body (cdddr exp)))
    (list 'begin
          (cons 'define
                (cons (cons proc vars) body))
          (cons proc exps))))

; test

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))
(fib 10)
;Value: 55

(eval '(define (fib n)
         (let fib-iter ((a 1)
                        (b 0)
                        (count n))
           (if (= count 0)
               b
               (fib-iter (+ a b) a (- count 1)))))
      the-global-environment)

(eval '(fib 10)
      the-global-environment)
;Value: 55
