
;constructor A
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define frame (make-frame 1 2 3))
(origin-frame frame)
;Value: 1
(edge1-frame frame)
;Value: 2
(edge2-frame frame)
;Value: 3

;constructor B
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

(define frame (make-frame 1 2 3))
(origin-frame frame)
(edge1-frame frame)
(edge2-frame frame)