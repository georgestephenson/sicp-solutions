; original version of expmod
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;Alyssa P. Hacker's suggested version
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; In the use case of fermat-test the exp is biggest than the base, 
; so the result of fast-expt will get extremely big.
; In reality this will hamper performance.

; The benefit of the defined version of expmod, as explained on page 52, footnote 46,
; Is that we recursively compute on remainders of modulo m, keeping the numbers small
; and simpler to compute, bounded by m.