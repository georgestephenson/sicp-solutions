(load "2-93.scm")

;;;;;;;;;;;;
;; PART A ;;
;;;;;;;;;;;;

; define pseudoremainder-terms
; modify gcd-terms to use pseudoremainder-terms

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (greatest-common-divisor n d)))
      (cons (div n g) (div d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'greatest-common-divisor '(scheme-number scheme-number)
       (lambda (n d) (tag (gcd n d))))
  'done)

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                  (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
              (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
          (let ((t1 (first-term L1)) (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                    (adjoin-term
                    t1 (add-terms (rest-terms L1) L2)))
                  ((< (order t1) (order t2))
                    (adjoin-term
                    t2 (add-terms L1 (rest-terms L2))))
                  (else
                    (adjoin-term
                    (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                    (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                  (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- SUB-POLY"
              (list p1 p2))))

  (define (negate-term t)
    (make-term (order t) (sub 0 (coeff t))))
  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L1) (map negate-term L2))
          ((empty-termlist? L2) L1)
          (else
          (let ((t1 (first-term L1)) (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                    (adjoin-term
                    t1 (sub-terms (rest-terms L1) L2)))
                  ((< (order t1) (order t2))
                    (adjoin-term
                    (negate-term t2) (sub-terms L1 (rest-terms L2))))
                  (else
                    (adjoin-term
                    (make-term (order t1)
                               (sub (coeff t1) (coeff t2)))
                    (sub-terms (rest-terms L1)
                               (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
              (list p1 p2))))
  (define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
          (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (div-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- DIV-POLY"
              (list p1 p2))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                      ; 1. multiply the result by the divisor L2
                      ; 2. subtract that from the dividend L1
                      ; 3. recursively divide the difference by the divisor L2
                      (div-terms (sub-terms L1
                                            (mul-terms (list (make-term new-o new-c)) 
                                                        L2))
                                  L2)))
                  ; add first term of the quotient to rest of the result
                  ; remainder will be whatever is left in the last iteration
                  (list (add-terms (list (make-term new-o new-c)) 
                                   (car rest-of-result)) 
                        (cadr rest-of-result))
                  ))))))
  (define (=zero?-poly p)
    (or (empty-termlist? (term-list p))
        (all-true? (map =zero? (map coeff (term-list p))))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (pseudoremainder-terms a b))))
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  (define (mul-terms-by-int terms int)
    (map (lambda (t) (make-term (order t) (* int (coeff t)))) terms))
  (define (pseudoremainder-terms a b)
    (define (int-factor p q)
      (let ((o1 (order (car p)))
            (o2 (order (car q)))
            (c (coeff (car q))))
        (expt c (+ 1 o1 (- o2)))))
    (remainder-terms (mul-terms-by-int a (int-factor a b)) b))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- GCD-POLY"
              (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2) (tag (div-poly p1 p2))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  'done)

(define (greatest-common-divisor n d) (apply-generic 'greatest-common-divisor n d))

(install-rational-package)
(install-scheme-number-package)
(install-polynomial-package)



(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)
;Value: (polynomial x (2 1458) (1 -2916) (0 1458))


;;;;;;;;;;;;
;; PART B ;;
;;;;;;;;;;;;

; modify gcd-terms to divide coefficients by integer greatest common divisor

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                  (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
              (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
          (let ((t1 (first-term L1)) (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                    (adjoin-term
                    t1 (add-terms (rest-terms L1) L2)))
                  ((< (order t1) (order t2))
                    (adjoin-term
                    t2 (add-terms L1 (rest-terms L2))))
                  (else
                    (adjoin-term
                    (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                    (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                  (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- SUB-POLY"
              (list p1 p2))))

  (define (negate-term t)
    (make-term (order t) (sub 0 (coeff t))))
  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L1) (map negate-term L2))
          ((empty-termlist? L2) L1)
          (else
          (let ((t1 (first-term L1)) (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                    (adjoin-term
                    t1 (sub-terms (rest-terms L1) L2)))
                  ((< (order t1) (order t2))
                    (adjoin-term
                    (negate-term t2) (sub-terms L1 (rest-terms L2))))
                  (else
                    (adjoin-term
                    (make-term (order t1)
                               (sub (coeff t1) (coeff t2)))
                    (sub-terms (rest-terms L1)
                               (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
              (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                  (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
          (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (div-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- DIV-POLY"
              (list p1 p2))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                      ; 1. multiply the result by the divisor L2
                      ; 2. subtract that from the dividend L1
                      ; 3. recursively divide the difference by the divisor L2
                      (div-terms (sub-terms L1
                                            (mul-terms (list (make-term new-o new-c)) 
                                                        L2))
                                  L2)))
                  ; add first term of the quotient to rest of the result
                  ; remainder will be whatever is left in the last iteration
                  (list (add-terms (list (make-term new-o new-c)) 
                                   (car rest-of-result)) 
                        (cadr rest-of-result))
                  ))))))
  (define (=zero?-poly p)
    (or (empty-termlist? (term-list p))
        (all-true? (map =zero? (map coeff (term-list p))))))
  (define (gcd-coeffs terms)
    (if (= (length terms) 1)
        (coeff (car terms))
        (gcd (coeff (car terms)) (gcd-coeffs (cdr terms)))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (let ((terms (gcd-terms b (pseudoremainder-terms a b))))
          (mul-terms-by-int terms
                            (/ 1 (gcd-coeffs terms))))))
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  (define (mul-terms-by-int terms int)
    (map (lambda (t) (make-term (order t) (* int (coeff t)))) terms))
  (define (pseudoremainder-terms a b)
    (define (int-factor p q)
      (let ((o1 (order (car p)))
            (o2 (order (car q)))
            (c (coeff (car q))))
        (expt c (+ 1 o1 (- o2)))))
    (remainder-terms (mul-terms-by-int a (int-factor a b)) b))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- GCD-POLY"
              (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2) (tag (div-poly p1 p2))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  'done)

(install-polynomial-package)


(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)
;Value: (polynomial x (2 1) (1 -2) (0 1))