; prerequisites

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; part a

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
;Value: 4

; again, the example in this exercise is not a great test of the solution.

(deriv '((x * x) + (3 * (x * (y + 2)))) 'x)
;Value: ((x + x) + (3 * (y + 2)))


; part b

(define (sum? x)
  (and (pair? x) (memq '+ x)))

(define (product? x)
  (and (pair? x) (and (not (sum? x)) (memq '* x))))

(define (addend s) (before '+ s))

(define (multiplier p) (before '* p))

(define (augend s) (after '+ s))

(define (multiplicand p) (after '* p))

(define (get-exp exp)
  (if (and (pair? exp) 
           (null? (cdr exp)))
      (car exp)
      exp))

(define (after op exp) (get-exp (cdr (memq op exp))))

(define (before op exp)
  (define (cons-until-op exp)
    (if (eq? op (car exp))
        '()
        (cons (car exp) (cons-until-op (cdr exp)))))
  (get-exp (cons-until-op exp)))

(deriv '(x * x + 3 * x * y + 2 + x * (x * (x + y + 4) + x)) 'x)
;Value: ((x + x) + ((3 * y) + ((x * ((x + (x + y + 4)) + 1)) + (x * (x + y + 4) + x))))
; = 2x + (3y + (x (2x + y + 4 + 1)) + x^2 + yx + 4x + x)
; = 2x + 3y + 2x^2 + yx + 5x + x^2 + yx + 5x
; = 3x^2 + x(2y + 12) + 3y

; expected answer calculated from https://www.derivative-calculator.net/
; 3x^2 + x(2y + 12) + 3y

; the solution is not very simplified, but it does work.