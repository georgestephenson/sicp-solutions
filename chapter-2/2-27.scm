(define x (list (list 1 2) (list 3 4)))

x
;Value: ((1 2) (3 4))

(reverse x)
;Value: ((3 4) (1 2))

(define (deep-reverse items)
  (define check-item
    (lambda (x) 
      (if (not (pair? x)) x (deep-reverse x))))
  (let ((a (check-item (car items)))
        (b (check-item (cdr items))))
    (if (null? b)
        a
        (list b a))))

(deep-reverse x)
;Value: ((4 3) (2 1))