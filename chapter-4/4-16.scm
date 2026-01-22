; exercise 4.16

; load solution to exercise 4.6

(load "4-06.scm")

; we need to define unassigned

; part a

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (let ((val (car vals)))
               (if (eq? val '*unassigned*)
                   (error "Unassigned variable" var)
                   val)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; part b

(define (scan-out-defines procedure-body)
  (if (pair? procedure-body)
      (let ((defines (filter definition? procedure-body))
            (non-defines (filter (lambda (x) (not (definition? x)))
                                 procedure-body)))
        (if (null? defines)
            procedure-body
            (list
             (let->combination
              (list 'let
                    (map (lambda (var) (list var ''*unassigned*))
                         (map definition-variable defines))
                    (cons 'begin
                          (append
                           (map (lambda (d)
                                  (list 'set!
                                        (definition-variable d)
                                        (definition-value d)))
                                defines)
                           non-defines)))))))
      procedure-body))

; part c

; make-procedure is where are procedure structure is written,
; procedure-body is where the procedure body is read.
; so I prefer to redefine how the procedure is written

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

; test

(eval '((lambda ()
   (define x 10)
   x))
   the-global-environment)
;Value: 10

(eval '((lambda ()
   (define even?
     (lambda (n)
       (if (= n 0) true (odd? (- n 1)))))
   (define odd?
     (lambda (n)
       (if (= n 0) false (even? (- n 1)))))
   (even? 4)))
   the-global-environment)
;Value: #t

(eval
  '((lambda ()
      (define x y)
      (define y 10)
      x))
  the-global-environment)
;Unassigned variable y