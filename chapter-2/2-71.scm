(load "2-69.scm")

; I'll draw this with the code output, where the first value in each list is left-branch, and the second is right-branch.

(generate-huffman-tree 
  '((A 1) (B 2) (C 4) (D 8) (E 16)))
;(((((leaf a 1) 
;    (leaf b 2) (a b) 3) 
;   (leaf c 4) (a b c) 7) 
;  (leaf d 8) (a b c d) 15) 
; (leaf e 16) (a b c d e) 31)

(generate-huffman-tree 
  '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))
;((((((((((leaf a 1) 
;         (leaf b 2) (a b) 3)  
;        (leaf c 4) (a b c) 7) 
;       (leaf d 8) (a b c d) 15) 
;      (leaf e 16) (a b c d e) 31) 
;     (leaf f 32) (a b c d e f) 63) 
;    (leaf g 64) (a b c d e f g) 127) 
;   (leaf h 128) (a b c d e f g h) 255) 
;  (leaf i 256) (a b c d e f g h i) 511) 
; (leaf j 512) (a b c d e f g h i j) 1023)

; 1 bit is required to to encode the most frequent symbol.
; The least frequent symbol requires n-1 bits.