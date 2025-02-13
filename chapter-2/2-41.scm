(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
      
(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j) 
               (map (lambda (k) (list i j k))
                    (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-triples 6)
;Value: ((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3) (6 2 1) (6 3 1) (6 3 2) (6 4 1) (6 4 2) (6 4 3) (6 5 1) (6 5 2) (6 5 3) (6 5 4))

(define (triple-sum-to? s triple)
  (= (+ (car triple) (cadr triple) (caddr triple)) s))

(define (unique-triples-sum-to s n)
  (filter (lambda (triple) (triple-sum-to? s triple))
          (unique-triples n)))

(unique-triples-sum-to 8 6)
;Value: ((4 3 1) (5 2 1))