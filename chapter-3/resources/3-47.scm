
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))  ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

; ; semaphore in terms of mutexes
; (define (make-semaphore n)
;   (let ((count n)
;         (the-mutex (make-mutex)))
;     (define (the-semaphore m)
;       (cond ((eq? m 'acquire)
;              (the-mutex 'acquire)
;              (if (zero? count)
;                  (begin
;                    (the-mutex 'release)
;                    (the-semaphore 'acquire))
;                  (begin
;                    (set! count (- count 1))
;                    (the-mutex 'release))))
;             ((eq? m 'release)
;              (the-mutex 'acquire)
;              (if (= count n)
;                  (the-mutex 'release)
;                  (begin
;                    (set! count (+ count 1))
;                    (the-mutex 'release))))))
;     the-semaphore))

; semaphore in terms of atomic test-and-set! operations
(define (make-semaphore n)
  (let ((count n)
        (cell (list false)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-semaphore 'acquire)
                 (if (zero? count)
                     (begin
                       (clear! cell)
                       (the-semaphore 'acquire))
                     (begin
                       (set! count (- count 1))
                       (clear! cell)))))
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (the-semaphore 'release)
                 (if (= count n)
                     (clear! cell)
                     (begin
                       (set! count (+ count 1))
                       (clear! cell)))))))
    the-semaphore))

(define (test-parallel)
  (define s (make-semaphore 2))
  
  (define (worker id)
    (display (string-append "Thread " (number->string id) " acquiring\n"))
    (s 'acquire)
    (display (string-append "Thread " (number->string id) " GOT IT\n"))
    (sleep-current-thread 1000)
    (s 'release)
    (display (string-append "Thread " (number->string id) " released\n")))
  
  (create-thread #f (lambda () (worker 1)))
  (create-thread #f (lambda () (worker 2)))
  (create-thread #f (lambda () (worker 3)))
  
  ; Wait long enough for threads to complete
  (sleep-current-thread 5000)
  (display "Test finished\n"))

(test-parallel)