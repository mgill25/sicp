#lang scheme
; Newton's method for calculating square roots of numbers
; Radicand -> number whose square we are calculating.
; Guess -> keep improving this. If good enough, we stop,
; else, we make a better guess.
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
  guess   ; returns guess
  (sqrt-iter (improve guess x)
             x)))

; Define Improve
(define (improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

; What do we mean by good enough???
; A somewhat alright test...
; The idea is to improve the answer until it's close enough 
; so that its square differs from the radicand by 
; less than a predetermined tolerance (here 0.001)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; Finally, we need a way to get started.
; Guess that the square root of any number is 1.
(define (sqrt x)
  (sqrt-iter 1.0 x))

; square methods for the above test
(define (square x)
  (* x x))