(load "section-2-5-2.scm")

(define (identity x) x)

(define (apply-generic op . args)
  (define (no-method-err op type-tags)
    (error "No method for these types" (list op type-tags)))
  (define (all-true? lst)
    (if (null? lst)
        #t
        (and (car lst) (all-true? (cdr lst)))))
  (define (get-coerce-map type-tags try-type)
    (map (lambda (t) (if (eq? t try-type)
                         identity
                         (get-coercion t try-type))) 
         type-tags))
  (define (find-coerce-map type-tags)
    (define (find-coerce-map-iter type-tags remaining)
      (if (null? remaining) 
          #f
          (let ((next (get-coerce-map type-tags (car remaining))))
            (if (all-true? next)
                next
                (find-coerce-map-iter type-tags (cdr remaining))))))
    (find-coerce-map-iter type-tags type-tags))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coerce-map (find-coerce-map type-tags)))
            (if coerce-map
                (apply apply-generic op (map (lambda (f x) (f x)) coerce-map args))
                (no-method-err op type-tags)))))))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (add x y z) (apply-generic 'add x y z))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (add-complex-3 z1 z2 z3)
    (make-from-real-imag (+ (real-part z1) (real-part z2) (real-part z3))
                         (+ (imag-part z1) (imag-part z2) (imag-part z3))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'add '(complex complex complex)
       (lambda (z1 z2 z3) (tag (add-complex-3 z1 z2 z3))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-complex-package)

(make-complex-from-real-imag 1 2)
(make-scheme-number 7)

(apply-generic 'add 
  (make-complex-from-real-imag 1 2) 
  (make-scheme-number 7)
  (make-complex-from-real-imag 5 3))
;Value: (complex rectangular 13 . 5)