(define (or-gate o1 o2 output)
  (let ((no1 (make-wire))
        (no2 (make-wire))
        (and-output (make-wire)))
  (inverter o1 no1)
  (inverter o2 no2)
  (and-gate no1 no2 and-output)
  (inverter and-output output)
  'ok))

; the delay time will be 1 and-gate-delay and 2 inverter-delays
; the first two inverters can run in parallel