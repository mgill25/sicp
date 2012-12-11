#lang scheme
; Calculating factorial recursively
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
      

; Iteratively
(define (factorial2 n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; Even though factorial2 looks recursive (fact-iter calls itself), 
; it really is an iterative process. That's because the interpreter implements tail-recursion.
; The state of the process at any time is captured by the three state-variables
; product, counter, max-count. The process doesn't grow or shink.