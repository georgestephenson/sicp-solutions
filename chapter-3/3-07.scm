(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch try-password m)
    (if (eq? password try-password)
      (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m)))
      (lambda (x) "Incorrect password")))
  dispatch)

(define (make-joint account orig-password new-password)
  (define (dispatch try-password m)
    (if (eq? new-password try-password)
      (account orig-password m)
      (lambda (x) "Incorrect password")))
  dispatch)

(define peter-acc 
  (make-account 100 'open-sesame))

(define paul-acc 
  (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 40)
;Value: 60

((paul-acc 'open-sesame 'deposit) 50)
;Value: "Incorrect password"

((paul-acc 'rosebud 'deposit) 50)
;Value: 110

((peter-acc 'open-sesame 'withdraw) 10)
;Value: 100

((paul-acc 'rosebud 'withdraw) 25)
;Value: 75