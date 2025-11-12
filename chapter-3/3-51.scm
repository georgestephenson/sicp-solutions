(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
; 0
; prints 0 because first element evaluated immediately (printed immediately)

(stream-ref x 5)
; 1
; 2
; 3
; 4
; 5
; doesn't display 0 because the result of the first 'show'
; is remembered using memo-proc, and printing 0 is a side-effect
; of the initial evaluation, not a result
; calls 'show' up to n=5, prints remaining values up to 5

(stream-ref x 7)
; 6
; 7
; results up to 5 remembered by memo-proc, so aren't printed
; as printing was a side-effect of evaluating the procedure
; calls 'show' up to n=7, prints remaining values up to 7