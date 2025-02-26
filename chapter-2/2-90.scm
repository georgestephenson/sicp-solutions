(load "2-88.scm")

; in my implementation, only term-lists are different.
; our first-term selector for dense and sparse term-lists treats individual terms the same way.
; so we just need generic term-lists.

;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERIC TERM LISTS ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-dense-term-list-package)
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((= (length term-list) (order term)) 
            (cons (coeff term) term-list))
          ((> (- (length term-list) 1) (order term)) 
            (cons (car term-list) (adjoin-term term (cdr term-list))))
          (else (adjoin-term term (cons 0 term-list)))))
  (define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (negate-term t)
    (make-term (order t) (sub 0 (coeff t))))
  (define (negate-terms term-list)
    (map negate-term term-list))

  (define (tag x) (attach-tag 'dense x))
  (put 'adjoin-term '(dense) (lambda (term-list) (lambda (term) (tag (adjoin-term term term-list)))))
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense) (lambda (term-list) (tag (rest-terms term-list))))
  (put 'empty-termlist? '(dense) empty-termlist?)
  (put 'negate-terms '(dense) (lambda (term-list) (tag (negate-terms term-list))))
  (put 'make-term-list 'dense
       (lambda (x) (tag x))))

(define (install-sparse-term-list-package)
  (define (coeff term) (cadr term))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (negate-term t)
    (make-term (order t) (sub 0 (coeff t))))
  (define (negate-terms term-list)
    (map negate-term term-list))

  (define (tag x) (attach-tag 'sparse x))
  (put 'adjoin-term '(sparse) (lambda (term-list) (lambda (term) (tag (adjoin-term term term-list)))))
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) (lambda (term-list) (tag (rest-terms term-list))))
  (put 'empty-termlist? '(sparse) empty-termlist?)
  (put 'negate-terms '(sparse) (lambda (term-list) (tag (negate-terms term-list))))
  (put 'make-term-list 'sparse
       (lambda (x) (tag x))))

(install-dense-term-list-package)
(install-sparse-term-list-package)

(define (make-dense-term-list list)
  ((get 'make-term-list 'dense) list))

(define (make-sparse-term-list list)
  ((get 'make-term-list 'sparse) list))

; now we just need to make the minimal changes to implement generic term-lists

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REFACTOR POLYNOMIALS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    ((apply-generic 'adjoin-term term-list) term))

  (define (the-empty-termlist) '())
  (define (first-term term-list) 
    (newline)
    (display term-list)
    (apply-generic 'first-term term-list))
  (define (rest-terms term-list) 
    (apply-generic 'rest-terms term-list))
  (define (empty-termlist? term-list) 
    (apply-generic 'empty-termlist? term-list))

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
  (define (negate-terms term-list)
    (apply-generic 'negate-terms term-list))
  (define (sub-terms L1 L2)
    (newline)
    (display "L1 ")
    (display L1)
    (display " L2 ")
    (display L2)
    (cond ((empty-termlist? L1) (negate-terms L2))
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

(define poly1 (make-polynomial 'x (make-dense-term-list (list 4 7))))
poly1
;Value: (polynomial x dense 4 7)
(define poly2 (make-polynomial 'x (make-dense-term-list (list 3 1 34))))
poly2
;Value: (polynomial x dense 3 1 34)

(sub poly1 poly2)
;Value: (polynomial x dense -3 3 -27)

(sub poly2 poly1)
;Value: (polynomial x dense 3 -3 27)

(add poly1 poly2)
;Value: (polynomial x dense 3 5 41)


(define poly3 (make-polynomial 'x (make-sparse-term-list (list (list 1 4) (list 0 7)))))
(define poly4 (make-polynomial 'x (make-sparse-term-list (list (list 2 3) (list 1 1) (list 0 34)))))

(sub poly3 poly4)
;Value: (polynomial x sparse (2 -3) (1 3) (0 -27))
(sub poly4 poly3)
;Value: (polynomial x sparse (2 3) (1 -3) (0 27))