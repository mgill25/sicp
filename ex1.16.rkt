#lang scheme
;; Exercise 1.16
;; Design a proceedure that evolves an iterative exponentiation process that uses
;; successive squaring and uses a logarithmic number of steps, as does fast-expt
;; (Hint: Using the observation that (b^n/2)^2 = ((b^2)^(n/2)), keep, along with
;; the exponent n and the base b, an additional state variable a, and define the 
;; state transformation in such a way that the product a*b^n remains unchanged
;; from state to state. At the beginning of the process, a is take to be 1, and
;; the answer is given by the value of a at the end of the process.
;; In general, the technique of defining an `invarient quantity` that remains
;; unchanged from state to state is a powerful way to think about the design
;; of iterative algorithms.)

(define even? 
  (λ (n) 
    (= (remainder n 2) 0)))

(define square (λ(x) (* x x)))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)   
  (cond
    ((= counter 0) product)
    ((even? counter) (expt-iter (square b) (/ counter 2) product))
    (else (expt-iter b (- counter 1) (* product b)))))

; Sample run 1
; (expt 2 4) 
; (expt-iter 2 4 1)
; (expt-iter 4 2 1)
; (expt-iter 16 1 1)
; (expt-iter 16 0 16)
; 16

; Sample run 2
; (expt 3 3)
; (expt-iter 3 3 1)
; (expt-iter 3 2 3)
; (expt-iter 9 1 3)
; (expt-iter 9 0 27)
; 27