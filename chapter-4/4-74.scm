; exercise 4.74

(load "resources/ch4-query.scm")

; part a - definition of simple-flatten

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (s) (not (stream-null? s))) stream)))

(define (find-assertions pattern frame)
  (simple-stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (negate operands frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (lisp-value call frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(initialize-data-base microshaft-data-base)

(query-driver-loop)

; part b - testing query system's behaviour

(assert! 
  (rule (all-lives-near ?person-1 ?person-2)
        (and (lives-near ?person-1 ?person-2)
             (salary ?person-1 ?salary-1)
             (salary ?person-2 ?salary-2)
             (lisp-value < ?salary-1 ?salary-2))))

;;; Query input:
(all-lives-near ?person-1 ?person-2)
;;; Query results:
;(all-lives-near (aull dewitt) (reasoner louis))
;(all-lives-near (aull dewitt) (bitdiddle ben))
;(all-lives-near (reasoner louis) (bitdiddle ben))
;(all-lives-near (fect cy d) (hacker alyssa p))


(assert! 
  (rule (big-shot ?person ?division)
        (and (job ?person (?division . ?job-type))
             (not (and (supervisor ?person ?supervisor)
                       (job ?supervisor (?division . ?supervisor-job-type)))))))

;;; Query input:
(big-shot (Bitdiddle Ben) computer)
;;; Query results:
;(big-shot (bitdiddle ben) computer)


(assert! (friends Bill Ben))
(assert! (friends John Paul))

(assert! (rule (friends ?x ?y)
               (friends ?y ?x)))

(or (friends Bill ?x) (friends John ?y))

; (or (friends bill ben) (friends john ?y))
; (or (friends bill ?x) (friends john paul))
; (or (friends bill ben) (friends john ?y))
; (or (friends bill ?x) (friends john paul))
; repeats forever

; Overall it seems to behave as expected in these examples.
; It may have some effect on performance though.