; Original version of expmod
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; Louis Reasoner's version
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/exp 2) m)
                       (expmod base (/exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

; Rather than expmod recursively calling expmod at most once, half the time it will now call expmod twice.
; Both algorithms half the size of exp in each recursion if exp is even, this leads to O(log n) in the original.
; However Louis Reasoner's version will now double the numbers of calls to expmod n times.
; This will result in a recursion tree, of complexity O(log(2^n)), equivalent to O(n)