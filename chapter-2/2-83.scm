(load "resources/section-2-5-2.scm")

(define (raise x) (apply-generic 'raise x))

(put 'make 'integer (lambda (n) (attach-tag 'integer n)))
(define (make-integer n)
  ((get 'make 'integer) n))

(define (integer->rational n)
  (make-rational n 1))
(define (rational->scheme-number rat)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (make-scheme-number (/ (numer rat) (denom rat))))
(define (scheme-number->complex n)
  (make-complex-from-real-imag n 0))


(put 'raise '(integer) integer->rational)
(put 'raise '(rational) rational->scheme-number)
(put 'raise '(scheme-number) scheme-number->complex)

(define my-int (make-integer 7))
my-int
;Value: (integer . 7)

(define my-rat (raise my-int))
my-rat
;Value: (rational 7 . 1)

(define my-real (raise my-rat))
my-real
;Value: (scheme-number . 7)

(define my-complex (raise my-real))
my-complex
;Value: (complex rectangular 7 . 0)