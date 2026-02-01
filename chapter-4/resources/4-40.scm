; exercise 4.40

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

(define (multiple-dwelling)
  (let ((fletcher (amb 1 2 3 4 5)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (require (distinct? (list cooper fletcher)))
      (let ((miller (amb 1 2 3 4 5)))
        (require (> miller cooper))
        (require (distinct? (list cooper fletcher miller)))
        (let ((smith (amb 1 2 3 4 5)))
          (require (not (= (abs (- smith fletcher)) 1)))
          (require (distinct? (list cooper fletcher miller smith)))
          (let ((baker (amb 1 2 3 4 5)))
            (require (not (= baker 5)))
            (require (distinct? (list baker cooper fletcher miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))

;;; Amb-Eval input:
(multiple-dwelling)
;;; Starting a new problem 
;;; Amb-Eval value:
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
