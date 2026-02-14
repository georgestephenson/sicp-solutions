; exercise 4.69

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
(rule ((great great . ?rel) ?gg ?s)
      (and (son ?gg ?g)
           ((great . ?rel) ?g ?s)))
(rule ((great grandson) ?gg ?s)
      (and (grandson ?g ?s)
           (son ?gg ?g)))
))

(initialize-data-base genesis-data-base)

(query-driver-loop)

; test

((great grandson) ?g ?ggs)
;;; Query results:
;((great grandson) irad lamech)
;((great grandson) enoch methushael)
;((great grandson) cain mehujael)
;((great grandson) adam irad)
;((great grandson) mehujael jubal)
;((great grandson) mehujael jabal)

;;; Query input:
(?relationship Adam Irad)
;;; Query results:
;((great grandson) adam irad)

;;; Query input:
(?relationship Adam Jabal)
;;; Query results:
;((great great great great great grandson) adam jabal)