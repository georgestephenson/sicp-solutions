; exercise 4.24

(define (proc-time-test proc description)
  (newline)
  (display "Time taken - ")
  (display description)
  (display ":")
  (newline)
  (define start-time (runtime))
  (proc)
  (display (- (runtime) start-time))
  (display " seconds")
  (newline))

(load "resources/ch4-mceval.scm")

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

(proc-time-test (lambda () 
  (eval '((lambda (n)
            ((lambda (fib)
                (fib fib n))
              (lambda (fb k)
                (if (= k 0)
                    0
                    (if (= k 1)
                        1
                        (+ (fb fb (- k 2)) (fb fb (- k 1))))))))
            20)
      the-global-environment))
      "original metacircular evaluator")
;Time taken - original metacircular evaluator:
;3.15 seconds

(proc-time-test (lambda () 
  (eval '((lambda (n)
            ((lambda (fib)
                (fib fib n))
              (lambda (fb k)
                (if (= k 0)
                    0
                    (if (= k 1)
                        1
                        (+ (fb fb (- k 2)) (fb fb (- k 1))))))))
            25)
      the-global-environment))
      "original metacircular evaluator")
;Time taken - original metacircular evaluator:
;33.17 seconds

; reset environment
(ge (make-top-level-environment))

(define (proc-time-test proc description)
  (newline)
  (display "Time taken - ")
  (display description)
  (display ":")
  (newline)
  (define start-time (runtime))
  (proc)
  (display (- (runtime) start-time))
  (display " seconds")
  (newline))

(load "resources/ch4-analyzingmceval.scm")

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

(proc-time-test (lambda () 
  (eval '((lambda (n)
            ((lambda (fib)
                (fib fib n))
              (lambda (fb k)
                (if (= k 0)
                    0
                    (if (= k 1)
                        1
                        (+ (fb fb (- k 2)) (fb fb (- k 1))))))))
            20)
      the-global-environment))
      "analyzing metacircular evaluator")
;Time taken - analyzing metacircular evaluator:
;1.79 seconds

(proc-time-test (lambda () 
  (eval '((lambda (n)
            ((lambda (fib)
                (fib fib n))
              (lambda (fb k)
                (if (= k 0)
                    0
                    (if (= k 1)
                        1
                        (+ (fb fb (- k 2)) (fb fb (- k 1))))))))
            25)
      the-global-environment))
      "analyzing metacircular evaluator")
;Time taken - analyzing metacircular evaluator:
;21.660000000000004 seconds

; RESULTS

; Time taken - original metacircular evaluator:
; n=20, 3.15 seconds
; n=25, 33.17 seconds

; Time taken - analyzing metacircular evaluator:
; n=20, 1.79 seconds
; n=25, 21.660000000000004 seconds

; in the original version, analysis repeated n times


; (3.15-1.79)/3.15 = 43%
; (33.17-21.66)/33.17 = 35%

; In original evaluator, 35-43% of time can be saved moving analysis out of execution.