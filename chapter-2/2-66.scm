(define (lookup given-key set)
  (cond ((null? set) false)
        ((= given-key (key (entry set))) (entry set))
        ((< given-key (key (entry set)))
         (lookup given-key (left-branch set)))
        ((> given-key (key (entry set)))
         (lookup given-key (right-branch set)))))
