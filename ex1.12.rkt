#lang scheme

; Write a proceedure that computes elements of Pascal's 
; triangle by means of a recursive process.

; Should ideally only use the elements of scheme introduced
; by the book upto this point. So, gonna try without using lists.
; 0-index based
(define (pascal row col)
  (cond
    ((or (= row 1) (= col 0) (= col row)) 1)
    ((> col row) 0)
    ((< col 0) 0)
    (else  
      (+ (pascal (- row 1) (- col 1))
         (pascal (- row 1) col)))))
              
; Yay! :)
