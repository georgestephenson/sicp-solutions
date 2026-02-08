; exercise 4.63

(load "resources/ch4-query.scm")

(define genesis-data-base
  '(
(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)
(rule (grandson ?g ?s)
      (and (son ?f ?s)
           (son ?g ?f)))
(rule (son ?m ?s)
      (and (wife ?m ?w)
           (son ?w ?s)))
))

(initialize-data-base genesis-data-base)

(query-driver-loop)

; test

;;; Query input:
(grandson Cain ?grandson)
;;; Query results:
;(grandson cain irad)

;;; Query input:
(son Lamech ?son)
;;; Query results:
;(son lamech jubal)
;(son lamech jabal)

;;; Query input:
(grandson Methushael ?grandson)
;;; Query results:
;(grandson methushael jubal)
;(grandson methushael jabal)