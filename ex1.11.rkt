#lang scheme

; SICP Ex. 1.11
; A function f is defined by the rule that 
;     f(n) = n                             if n < 3 
;     f(n) = f(n-1) + 2f(n-2) + 3f(n-3)    if n >= 3
; Write a recursive and an iterative proceedure that computes f.

; Recursive
(define (f n)
  (cond
    ((< n 3) n)
    (else
     (+
      (f (- n 1))
      (* 2 (f (- n 2)))
      (* 3 (f (- n 3)))))))


; Linearly Iterative 
; This function is a modification of the fibonacci sequence! 
; When n >= 3; f(n) is equal to f(n-1) + 2f(n-2) + 3f(n-3)
; which are the previous 3 terms in the series! 

(define (f2 n)
  (f-iter 2 1 0 n))

; Keep shifting values and accumulating the result in a
; n is used as a counter
; a <- a + 2b + 3c
; b <- a
; c <- b
(define (f-iter a b c n)
  (if (= n 0)
      c    
      (f-iter (+ a
                 (* 2 b)
                 (* 3 c))
              a b (- n 1))))

; This was the solution I deviced first, but kept getting
; stuck on the n < 3 case.
; Clever way of letting the wrapper function deal with it, 
; since, notice that foo-iter only produces correct results
; for n > 3.
(define (f3 n)
  (define (foo-iter a b c n)
    (if (< n 3)
        a
        (foo-iter 
         (+ a (* 2 b) (* 3 c))
         a
         b
         (- n 1))))
  (if (< n 3) ; we deal with n < 3 cases here, so foo-iter doesn't deal with it
      n
      (foo-iter 2 1 0 n)))