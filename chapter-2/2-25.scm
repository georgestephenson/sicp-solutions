; Prepare the list values

(define list1 (list 1 3 (list 5 7) 9))
list1
;Value: (1 3 (5 7) 9)

(define list2 (list (list 7)))
list2
;Value: ((7))

(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
list3
;Value: (1 (2 (3 (4 (5 (6 7))))))


; Get the 7 values using car and cdr sequences

(car (cdr (car (cdr (cdr list1)))))
;Value: 7

(car (car list2))
;Value: 7

(car (cdr 
  (car (cdr 
    (car (cdr 
      (car (cdr 
        (car (cdr 
          (car (cdr list3))))))))))))
;Value: 7