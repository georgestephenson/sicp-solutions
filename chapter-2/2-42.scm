(load "resources/listprocs.scm")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (diagonal? check-pos k positions)
  (define (last-is val idx inc)
    (cond ((= idx -1) #f) 
          ((= val 0) #f)
          ((> val 8) #f)  
          ((= val (list-ref positions idx)) #t)
          (else (last-is (inc val) (- idx 1) inc))))
  (or (last-is (- check-pos 1) (- k 2) (lambda (x) (- x 1)))
      (last-is (+ check-pos 1) (- k 2) (lambda (x) (+ x 1)))))

; should be #t
(diagonal? 4 4 (list 1 2 3 4))
(diagonal? 4 4 (list 3 2 1 4))
(diagonal? 1 4 (list 2 3 4 1))

; should be #f
(diagonal? 2 4 (list 1 2 4 2))
(diagonal? 4 4 (list 2 3 1 4))
(diagonal? 1 4 (list 2 4 3 1))

(define (safe? k positions)
  (let ((check-pos (list-ref positions (- k 1))))
    (and (= 1 (length (filter (lambda (x) (= x check-pos)) positions)))
         (not (diagonal? check-pos k positions)))))

; should be #t
(safe? 4 (list 4 4 4 8))
(safe? 4 (list 1 3 6 8))
(safe? 5 (list 2 3 6 8 5))
(safe? 8 (list 3 3 3 3 3 8 7 1))

; should be #f
(safe? 8 (list 4 4 4 4 4 4 4 4))
(safe? 8 (list 4 4 4 4 4 4 7 8))
(safe? 8 (list 1 2 3 4 4 4 4 8))
(safe? 6 (list 4 5 3 4 4 1))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(length (queens 8))
;Value: 92

(queens 8)
;Value: ((1 5 8 6 3 7 2 4) (1 6 8 3 7 4 2 5) (1 7 4 6 8 2 5 3) (1 7 5 8 2 4 6 3) (2 4 6 8 3 1 7 5) (2 5 7 1 3 8 6 4) (2 5 7 4 1 8 6 3) (2 6 1 7 4 8 3 5) (2 6 8 3 1 4 7 5) (2 7 3 6 8 5 1 4) (2 7 5 8 1 4 6 3) (2 8 6 1 3 5 7 4) (3 1 7 5 8 2 4 6) (3 5 2 8 1 7 4 6) (3 5 2 8 6 4 7 1) (3 5 7 1 4 2 8 6) (3 5 8 4 1 7 2 6) (3 6 2 5 8 1 7 4) (3 6 2 7 1 4 8 5) (3 6 2 7 5 1 8 4) (3 6 4 1 8 5 7 2) (3 6 4 2 8 5 7 1) (3 6 8 1 4 7 5 2) (3 6 8 1 5 7 2 4) (3 6 8 2 4 1 7 5) (3 7 2 8 5 1 4 6) (3 7 2 8 6 4 1 5) (3 8 4 7 1 6 2 5) (4 1 5 8 2 7 3 6) (4 1 5 8 6 3 7 2) (4 2 5 8 6 1 3 7) (4 2 7 3 6 8 1 5) (4 2 7 3 6 8 5 1) (4 2 7 5 1 8 6 3) (4 2 8 5 7 1 3 6) (4 2 8 6 1 3 5 7) (4 6 1 5 2 8 3 7) (4 6 8 2 7 1 3 5) (4 6 8 3 1 7 5 2) (4 7 1 8 5 2 6 3) (4 7 3 8 2 5 1 6) (4 7 5 2 6 1 3 8) (4 7 5 3 1 6 8 2) (4 8 1 3 6 2 7 5) (4 8 1 5 7 2 6 3) (4 8 5 3 1 7 2 6) (5 1 4 6 8 2 7 3) (5 1 8 4 2 7 3 6) (5 1 8 6 3 7 2 4) (5 2 4 6 8 3 1 7) (5 2 4 7 3 8 6 1) (5 2 6 1 7 4 8 3) (5 2 8 1 4 7 3 6) (5 3 1 6 8 2 4 7) (5 3 1 7 2 8 6 4) (5 3 8 4 7 1 6 2) (5 7 1 3 8 6 4 2) (5 7 1 4 2 8 6 3) (5 7 2 4 8 1 3 6) (5 7 2 6 3 1 4 8) (5 7 2 6 3 1 8 4) (5 7 4 1 3 8 6 2) (5 8 4 1 3 6 2 7) (5 8 4 1 7 2 6 3) (6 1 5 2 8 3 7 4) (6 2 7 1 3 5 8 4) (6 2 7 1 4 8 5 3) (6 3 1 7 5 8 2 4) (6 3 1 8 4 2 7 5) (6 3 1 8 5 2 4 7) (6 3 5 7 1 4 2 8) (6 3 5 8 1 4 2 7) (6 3 7 2 4 8 1 5) (6 3 7 2 8 5 1 4) (6 3 7 4 1 8 2 5) (6 4 1 5 8 2 7 3) (6 4 2 8 5 7 1 3) (6 4 7 1 3 5 2 8) (6 4 7 1 8 2 5 3) (6 8 2 4 1 7 5 3) (7 1 3 8 6 4 2 5) (7 2 4 1 8 5 3 6) (7 2 6 3 1 4 8 5) (7 3 1 6 8 5 2 4) (7 3 8 2 5 1 6 4) (7 4 2 5 8 1 3 6) (7 4 2 8 6 1 3 5) (7 5 3 1 6 8 2 4) (8 2 4 1 7 5 3 6) (8 2 5 3 1 7 4 6) (8 3 1 6 2 5 7 4) (8 4 1 3 6 2 7 5))