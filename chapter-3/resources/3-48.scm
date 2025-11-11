; prerequisites

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (without-interrupts
   (lambda ()
     (if (car cell)
         true
         (begin (set-car! cell true)
                false)))))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

; changed procedures

(define (make-account balance account-number)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'account-number) account-number)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (account-number1 (account1 'account-number))
        (account-number2 (account2 'account-number)))
    (if (< account-number1 account-number2)
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account1
         account2))
    ))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (account-number1 (account1 'account-number))
        (account-number2 (account2 'account-number)))
    (if (< account-number1 account-number2)
        ((serializer1
          (lambda ()
            ;; After getting first lock, sleep to give other thread a chance
            (sleep-current-thread 100)  ; 100ms delay
            ((serializer2 exchange)
            account1
            account2))))
        ((serializer2 
          (lambda ()
            ;; After getting first lock, sleep to give other thread a chance
            (sleep-current-thread 100)  ; 100ms delay
            ((serializer1 exchange)
            account1
            account2)))))
    ))

; (define (serialized-exchange account1 account2)
;   (let ((serializer1 (account1 'serializer))
;         (serializer2 (account2 'serializer)))
;     ((serializer1 
;       (lambda ()
;         ;; After getting first lock, sleep to give other thread a chance
;         (sleep-current-thread 100)  ; 100ms delay
;         ((serializer2 exchange)
;          account1
;          account2))))))

; testing

(define acc1 (make-account 100 1))
(define acc2 (make-account 200 2))

(define t1 
  (create-thread #f 
    (lambda ()
      (display "Thread 1 starting\n")
      (serialized-exchange acc1 acc2)
      (display "Thread 1 done\n"))))

(define t2
  (create-thread #f
    (lambda ()
      (display "Thread 2 starting\n")
      (serialized-exchange acc2 acc1)  ; opposite order!
      (display "Thread 2 done\n"))))

;; Give control to the threads so they can run
(yield-current-thread)
(sleep-current-thread 2000)  ; sleep for 1 second (1000 ms)

;; Check if they're still alive (deadlocked)
(thread-dead? t1)  ; should return #f if deadlocked
(thread-dead? t2)  ; should return #f if deadlocked