; exercise 4.67
; rather than describing what is required - I haven't implemented the loop detector below

; setup microshaft database and query driver loop

(load "resources/ch4-query.scm")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           ;; [extra newline at end] (announce-output output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream '()) '())))
           (query-driver-loop)))))

(define (qeval query frame-stream processed)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream processed)
        (simple-query query frame-stream processed))))

;;;Compound queries

(define (conjoin conjuncts frame-stream processed)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream
                      processed)
               processed)))

;;(put 'and 'qeval conjoin)


(define (disjoin disjuncts frame-stream processed)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream processed)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream
                       processed)))))

;;(put 'or 'qeval disjoin)

;;;Filters

(define (negate operands frame-stream processed)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)
                              processed))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

;;(put 'not 'qeval negate)

;;;Simple queries

(define (simple-query query-pattern frame-stream processed)
  (stream-flatmap
   (lambda (frame)
     (let ((instantiated (instantiate query-pattern 
                                     frame 
                                     (lambda (v f) v))))
       (if (member-similar? instantiated processed)
           (error "loop detected -- QEVAL" query-pattern)
           (stream-append-delayed
            (find-assertions query-pattern frame)
            (delay (apply-rules query-pattern frame 
                               (cons instantiated processed)))))))
   frame-stream))

(define (member-similar? item lst)
  (cond ((null? lst) false)
        ((same-structure? item (car lst)) true)
        (else (member-similar? item (cdr lst)))))

(define (same-structure? exp1 exp2)
  (cond ((and (var? exp1) (var? exp2)) true)
        ((or (var? exp1) (var? exp2)) false)
        ((and (pair? exp1) (pair? exp2))
         (and (same-structure? (car exp1) (car exp2))
              (same-structure? (cdr exp1) (cdr exp2))))
        (else (equal? exp1 exp2))))

(define (apply-rules pattern frame processed)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame processed))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame processed)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result)
                 processed)))))

(initialize-data-base microshaft-data-base)

(query-driver-loop)

; ; base case: should succeed
; (assert! 
;   (rule (outranked-by ?staff-person ?boss)
;       (or (supervisor ?staff-person ?boss)
;           (and (supervisor ?staff-person ?middle-manager)
;                (outranked-by ?middle-manager ?boss)))))

; ;;; Query input:
; (outranked-by (Bitdiddle Ben) ?who)
; ;;; Query results:
; (outranked-by (bitdiddle ben) (warbucks oliver))
; (outranked-by (bitdiddle ben) (warbucks oliver))

; test case: should detect loop
(assert! 
  (rule (outranked-by ?staff-person ?boss)
        (or (supervisor ?staff-person ?boss)
            (and (outranked-by ?middle-manager ?boss)
                 (supervisor ?staff-person ?middle-manager)))))

;;; Query input:
(outranked-by (Bitdiddle Ben) ?who)
;;; Query results:
(outranked-by (bitdiddle ben) (warbucks oliver))
(outranked-by (bitdiddle ben) (warbucks oliver))
;loop detected -- QEVAL (outranked-by (? 3 middle-manager) (? 3 boss))