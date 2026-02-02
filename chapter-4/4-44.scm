; exercise 4.44

(load "resources/ch4-ambeval.scm")

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (calc-diag-up cols n)
  (if (null? cols)
      '()
      (cons (- (car cols) n)
            (calc-diag-up (cdr cols) (+ n 1)))))

(define (calc-diag-down cols n)
  (if (null? cols)
      '()
      (cons (+ (car cols) n)
            (calc-diag-down (cdr cols) (+ n 1)))))

(define (queens)
  (define (q k)
    (if (= k 0)
        '()
        (let ((rest (q (- k 1))))
          (let ((col (amb 1 2 3 4 5 6 7 8)))
            (require (not (member col rest)))
            (let ((diag-up (calc-diag-up (cons col rest) 1))
                  (diag-down (calc-diag-down (cons col rest) 1)))
              (require (distinct? diag-up))
              (require (distinct? diag-down))
              (cons col rest))))))
  (q 8))

;;; Amb-Eval input:
(queens)
;;; Starting a new problem 
;;; Amb-Eval value:
;(4 2 7 3 6 8 5 1)