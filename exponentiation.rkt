#lang scheme
; Exponentiation

; Recursive proceedure to compute exponential of a given
; number b with the base n.

; We can say, b^n = b * b^(n-1), with b^0 = 1 as the base case.
; This can be converted to the following function
(define expt (λ(b n) 
  (if (= n 0)
      1
      (* b (expt b (- n 1))))))
; (expt 2 3) => 8
; Linear Recursive process, requiring O(n) steps and O(n) space.

; Linearly Iterative process:
(define (expt2 b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))
; Requires O(n) steps, and O(1) space. 
     
; We can use the method of successive squaring to calculate
; exponentials faster. Rather than doing
; b^6 as b.b.b.b.b.b, we do
; b^2 = b.b; b^4 = b^2 * b^2; b^6 = b^2 * b^2 * b^2, and so on...

; This method works fine for powers of 2. 
; We can make it general using the rule: 
; b^n = ((b^n/2)^2) if n is even
; b^n = b.b^(n-1) if n is odd.

(define fast-expt 
  (λ(b n)
    (cond
      ((= n 0) 1)
      ((even? n) (square (fast-expt b (/ n 2))))
      (else (* b (fast-expt b (- n 1)))))))
; where
(define (square x) (* x x))

; Complexity
; fast-expt grows logarithmically with n in both space and time.

; Sample run from the recursive proceedure
; (fast-expt 2 3)
; (* 2 (fast-expt 2 2)))
; (* 2 (square (fast-expt 2 1))))
; (* 2 (square (* 2 (fast-expt 2 0)))))
; (* 2 (square (* 2 1))))
; (* 2 (square 2)))
; (* 2 4))
; 8