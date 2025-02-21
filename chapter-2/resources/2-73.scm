;;;;;;;;;;;;
;; PART B ;;
;;;;;;;;;;;;

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package)
  ;; internal procedures
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (deriv-product operands var)
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (make-product m1 m2) (list '* m1 m2))

  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  'done)

(install-deriv-package)
(deriv '(* (* x y) (+ x 3)) 'x)
;Value: (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

;;;;;;;;;;;;
;; PART C ;;
;;;;;;;;;;;;

(define (install-deriv-package)
  ;; internal procedures
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (deriv-product operands var)
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (deriv-exponentiation operands var)
    (make-product (exponent operands)
                  (make-product (make-exponentiation (base operands) 
                                                     (make-sum (exponent operands) -1))
                                (deriv (base operands) var))))
  (define (base p) (car p))
  (define (exponent p) (cadr p))
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (make-product m1 m2) (list '* m1 m2))
  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation)
  'done)

(install-deriv-package)
(deriv '(+ (** x 5) (+ (* x y) (+ x 7))) 'x)
;Value: (+ (* 5 (* (** x (+ 5 -1)) 1)) (+ (+ (* x 0) (* 1 y)) (+ 1 0)))

;;;;;;;;;;;;
;; PART D ;;
;;;;;;;;;;;;

(define (install-deriv-package)
  ;; internal procedures
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (deriv-product operands var)
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (deriv-exponentiation operands var)
    (make-product (exponent operands)
                  (make-product (make-exponentiation (base operands) 
                                                     (make-sum (exponent operands) -1))
                                (deriv (base operands) var))))
  (define (base p) (car p))
  (define (exponent p) (cadr p))
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (make-product m1 m2) (list '* m1 m2))
  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  ;; interface to the rest of the system
  (put '+ 'deriv deriv-sum)
  (put '* 'deriv deriv-product)
  (put '** 'deriv deriv-exponentiation)
  'done)

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get (operator exp) 'deriv) (operands exp)
                                            var))))

(install-deriv-package)
(deriv '(+ (** x 5) (+ (* x y) (+ x 7))) 'x)
;Value: (+ (* 5 (* (** x (+ 5 -1)) 1)) (+ (+ (* x 0) (* 1 y)) (+ 1 0)))