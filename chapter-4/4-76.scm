; exercise 4.76


(load "resources/ch4-query.scm")

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin-frames
       (qeval (first-conjunct conjuncts) frame-stream)
       (conjoin (rest-conjuncts conjuncts) frame-stream))))

; I needed some help figuring out how to merge the two frame streams and each two frames (cartesian product)

(define (conjoin-frames frame-stream-1 frame-stream-2)
  (stream-flatmap
   (lambda (frame1)
     (stream-flatmap
      (lambda (frame2)
        (let ((merged (merge-frames frame1 frame2)))
          (if merged
              (singleton-stream merged)
              the-empty-stream)))
      frame-stream-2))
   frame-stream-1))

(define (merge-frames frame1 frame2)
  (define (merge-iter f1 f2)
    (cond ((eq? f2 'failed) 'failed)
          ((null? f1) f2)
          (else
           (let* ((binding (car f1))
                  (var (binding-variable binding))
                  (val (binding-value binding)))
             (merge-iter (cdr f1)
                        (extend-if-possible var val f2))))))
  (let ((result (merge-iter frame1 frame2)))
    (if (eq? result 'failed)
        #f
        result)))

; test

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


;;; Query input:
(grandson Methushael ?gs)
;;; Query results:
;(grandson methushael jubal)
;(grandson methushael jabal)
; it never actually terminates... but it does get the right solutions