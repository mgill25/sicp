#lang scheme
; Exercise 1.8 - Using Newton's method to calculate Cube roots.
(define (cube-root x)
  (define (cube-iter guess)
    (if (good-enough? guess)
        guess
        (cube-iter (improve guess))))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (square x) (* x x))
  (define (cube x) (* x x x))
  (cube-iter 1.0))
  