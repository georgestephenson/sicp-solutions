(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Normal-order evaluation
; Ignoring the expansion of ifs, each recursive call to gcd looks like

(gcd 206 40)

(gcd 40 (remainder 206 40))

(gcd (remainder 206 40) 
     (remainder 40 (remainder 206 40)))

(gcd (remainder 40 (remainder 206 40))  
     (remainder (remainder 206 40) 
                (remainder 40 (remainder 206 40))))

(gcd (remainder (remainder 206 40) 
                (remainder 40 (remainder 206 40))) 
     (remainder (remainder 40 (remainder 206 40)) 
                (remainder (remainder 206 40) 
                           (remainder 40 (remainder 206 40)))))

; Each of these calls to gcd requires the if operation to evaluate the "b" parameter every time
; Counting all of the remainder operations in each "b" parameter gives 0+1+2+4+7 = 14 calls
; The "a" parameter will only be evaluated in the final call 4 times giving 14+4 = 18 calls

; Applicative-order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2
; Applicative-order remainder is performed 4 times