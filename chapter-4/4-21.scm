; exercise 4.21

; part a

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)
;Value: 3628800

((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fb k)
      (if (= k 0)
          0
          (if (= k 1)
              1
              (+ (fb fb (- k 2)) (fb fb (- k 1))))))))
 10)
 ;Value: 55

 ((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fb k)
      (if (= k 0)
          0
          (if (= k 1)
              1
              (+ (fb fb (- k 2)) (fb fb (- k 1))))))))
 11)
 ;Value: 89

  ((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fb k)
      (if (= k 0)
          0
          (if (= k 1)
              1
              (+ (fb fb (- k 2)) (fb fb (- k 1))))))))
 12)
 ;Value: 144


 ; part b

 (define (f x)
   ((lambda (even? odd?)
      (even? even? odd? x))
    (lambda (ev? od? n)
      (if (= n 0) true (od? ev? od? (- n 1))))
    (lambda (ev? od? n)
      (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 0)
;Value: #t
(f 1)
;Value: #f
(f 23)
;Value: #f
(f 68)
;Value: #t