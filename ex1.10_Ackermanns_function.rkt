#lang scheme
; Exercise 1.10
; Ackermann's function
(define  (A x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else
     (A (- x 1)
        (A x (- y 1))))))

; Other definition? 
(define (A1 x y)
  (cond ((= x 0) (+ y 1))        
        ((= y 0) (A (- x 1) 1))
        (else (A (- x 1)
                 (A x (- y 1))))))
