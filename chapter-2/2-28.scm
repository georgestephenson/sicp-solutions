(define (fringe x)
  (if (not (pair? x)) 
      (list x)
      (let ((a (car x))
            (b (cdr x)))
        (if (null? b)
            (fringe a)
            (append (fringe a) 
                    (fringe b))))))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
;Value: (1 2 3 4)

(fringe (list x x))
;Value: (1 2 3 4 1 2 3 4)