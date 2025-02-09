

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(cc 100 us-coins)
;Value: 292

(cc 100 uk-coins)
;Value: 104561

; amusingly, the British halfpenny was demontinised in 1984 so this 
; was already out of date by the time SICP was published in 1985.

(define uk-coins (list 100 50 20 10 5 2 1))
(cc 100 uk-coins)
;Value: 4563

; try changing the order

(define uk-coins (list 50 5 20 2 100 10 1))
(cc 100 uk-coins)
;Value: 4563

; It doesn't matter because the procedure tries all combinations in the list
; Building the tree of all possible answers as we demonstrated in Exercise 1.14