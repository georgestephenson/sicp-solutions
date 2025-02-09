

(define (same-parity . items)
  (define get-item-rem2 (lambda (x) (remainder (car x) 2)))
  (define (same-parity-iter items result rem2)
    (if (null? items) 
        result
        (same-parity-iter (cdr items) 
                          (if (= (get-item-rem2 items) rem2) (append result (list (car items))) result)
                          rem2)))
  (same-parity-iter items (list) (get-item-rem2 items)))

(same-parity 1 2 3 4 5 6 7)
;Value: (1 3 5 7)
(same-parity 2 3 4 5 6 7)
;Value: (2 4 6)