; I was able to muster the naive approach here by making a 2nd polynomial variable
; the zeroeth term of the first polynomial. The would lead to a large expansion
; of terms in more complicated scenarios. I won't attempt to simplify terms!

(load "2-87.scm")

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

  (define (coerce psource ptarget)
    (make-poly (variable ptarget)
               (list (make-term 0 (tag psource)))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (add-poly p1 (coerce p2 p1))))

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

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                  (mul-terms (term-list p1)
                              (term-list p2)))
        (mul-poly p1 (coerce p2 p1))))

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

  (define (number-to-poly n p)
    (make-poly (variable p) (list (make-term 0 n))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'add '(polynomial scheme-number) 
       (lambda (p n) (tag (add-poly p (number-to-poly n p)))))
  (put 'add '(scheme-number polynomial) 
       (lambda (n p) (tag (add-poly (number-to-poly n p) p))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'mul '(polynomial scheme-number) 
       (lambda (p n) (tag (mul-poly p (number-to-poly n p)))))
  (put 'mul '(scheme-number polynomial) 
       (lambda (n p) (tag (mul-poly (number-to-poly n p) p))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero?-poly)
  'done)

(install-polynomial-package)
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))

(define poly1 (make-polynomial 'x (list (list 1 4) (list 0 7))))
(define poly2 (make-polynomial 'y (list (list 3 2) (list 2 4))))

(add poly1 poly2)
;Value: (polynomial x (1 4) (0 (polynomial y (3 2) (2 4) (0 7))))
(mul poly1 poly2)
;Value: (polynomial x (1 (polynomial y (3 8) (2 16))) (0 (polynomial y (3 14) (2 28))))