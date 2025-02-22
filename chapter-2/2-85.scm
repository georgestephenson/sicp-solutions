; this exercise seems to ignore rational numbers so I've removed that package

(load "resources/section-2-5-2.scm")

(define (raise x) (apply-generic 'raise x))
(define (equ? x y) (apply-generic 'equ? x y))
(define (project x) (apply-generic 'project x))
(define (drop x) 
  (let ((projection (project x)))
    (if (and projection (equ? (raise projection) x))
        (drop projection)
        x)))

(define (higher? type1 type2)
  ((get 'higher? type2) type1))

(define (install-integer-package)
  (define (integer->scheme-number n)
    (make-scheme-number n))
  (define (tag x)
    (attach-tag 'integer x))
  (put 'raise '(integer) integer->scheme-number)
  (put 'make 'integer (lambda (n) (attach-tag 'integer n)))
  (put 'higher? 'integer 
    (lambda (x) (or (eq? x 'scheme-number) ((get 'higher? 'scheme-number) x))))  
  (put 'project '(integer)
    (lambda (x) #f))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-scheme-number-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag n 0))
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
  (put 'raise '(scheme-number) scheme-number->complex)
  (put 'higher? 'scheme-number 
    (lambda (x) (or (eq? x 'complex) ((get 'higher? 'complex) x))))
  (put 'equ? '(scheme-number scheme-number)
    (lambda (x y) (= x y)))
  (put 'project '(scheme-number)
    (lambda (x) (make-integer (round x))))
  'done)

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
  (put 'higher? 'complex 
    (lambda (x) #f))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (and (= (real-part z1) (real-part z2)) 
                            (= (imag-part z1) (imag-part z2)))))
  (put 'project '(complex)
    (lambda (x) (make-scheme-number (real-part x))))
  'done)

(install-integer-package)
(install-scheme-number-package)
(install-complex-package)

;; Testing that drop works as expected

(drop (make-complex-from-real-imag 5 0))
;Value: (integer . 5)

(drop (make-complex-from-real-imag 5 4))
;Value: (complex rectangular 5 . 4)

(drop (make-complex-from-real-imag 5.65346 0))
;Value: (scheme-number . 5.65346)


;; Refactor apply-generic to use drop to simplify answers

(define (apply-generic op . args)
  (define (try-drop x)
    (if (or (eq? op 'add) (eq? op 'sub) (eq? op 'mul) (eq? op 'div))
        (drop x)
        x))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (try-drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (cond ((higher? type1 type2) (apply-generic op a1 (raise a2)))
                      ((higher? type2 type1) (apply-generic op (raise a1) a2))
                      (else (error "No method for these types" (list op type-tags)))))
              (error "No method for these types" (list op type-tags)))))))

(apply-generic 'add (make-complex-from-real-imag 5.65346 0) (make-complex-from-real-imag 65 0))
;Value: (scheme-number . 70.65346)
(apply-generic 'add (make-complex-from-real-imag 65 31) (make-complex-from-real-imag 43.452 0))
;Value: (complex rectangular 108.452 . 31)
(apply-generic 'add (make-complex-from-real-imag 12 0) (make-scheme-number 43))
;Value: (integer . 55)