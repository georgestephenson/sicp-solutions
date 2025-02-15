(define (equal? x y)
  (let ((xpair? (pair? x))
        (ypair? (pair? y)))
    (cond ((and xpair? ypair?) (and (equal? (car x) (car y)) 
                                      (equal? (cdr x) (cdr y))))
          ((not (or xpair? ypair?)) (eq? x y))
          (else #f))))

(equal? '(this is a list) '(this is a list))
;Value: #t
(equal? '(this is a list) '(this (is a) list))
;Value: #f