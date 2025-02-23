(load "2-88.scm")

;; Chose to implement this by keeping the same representation of term as a pair of order and coeff.
;; We construct this when we select first-term from the list of coeffs
;; And we collapse it when we adjoin-term back to the term-list.

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
    (cond ((=zero? (coeff term)) term-list)
          ((= (length term-list) (order term)) 
            (cons (coeff term) term-list))
          ((> (- (length term-list) 1) (order term)) 
            (cons (car term-list) (adjoin-term term (cdr term-list))))
          (else (adjoin-term term (cons 0 term-list)))))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))
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

  (define (=zero?-poly p)
    (or (empty-termlist? (term-list p))
        (all-true? (map =zero? (map coeff (term-list p))))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero?-poly)
  'done)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))

(install-polynomial-package)

(define poly1 (make-polynomial 'x (list 4 7)))
(define poly2 (make-polynomial 'x (list 3 1 34)))

(sub poly1 poly2)
;Value: (polynomial x -3 3 -27)

(sub poly2 poly1)
;Value: (polynomial x 3 -3 27)

(add poly1 poly2)
;Value: (polynomial x 3 5 41)