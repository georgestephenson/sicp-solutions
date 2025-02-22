(load "2-83.scm")

(define (higher? type1 type2)
  ((get 'higher? type2) type1))

(put 'higher? 'integer 
  (lambda (x) (or (eq? x 'rational) ((get 'higher? 'rational) x))))
(put 'higher? 'rational 
  (lambda (x) (or (eq? x 'scheme-number) ((get 'higher? 'scheme-number) x))))
(put 'higher? 'scheme-number 
  (lambda (x) (or (eq? x 'complex) ((get 'higher? 'complex) x))))
(put 'higher? 'complex 
  (lambda (x) #f))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (cond ((higher? type1 type2) (apply-generic op a1 (raise a2)))
                      ((higher? type2 type1) (apply-generic op (raise a1) a2))
                      (else (error "No method for these types" (list op type-tags)))))
              (error "No method for these types" (list op type-tags)))))))

(apply-generic 'add (make-rational 2 5) (make-complex-from-real-imag 7 9))
;Value: (complex rectangular 37/5 . 9)