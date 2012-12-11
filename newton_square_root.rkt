#!/usr/bin/env racket
#lang racket

; We now calculate the square root of a number using Newton's approximation method.
; We take a guess y for the square root of a number x, and then simly average the guess y with the number x to obtain the next guess. As we continue this process, we obtain better and better approximatioin for the square root of the number.

; This can be translated into the following basic strategy as a procedure.
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
  guess
  (sqrt-iter (improve guess x)
              x)))

;   A guess is improved by averaging it with the quotient of the radicand and the old guess.
;   (define (improve guess x)
;       (average guess (/ x guess)))
;   where...
;   (define (average x y)
;       (/ (+ x y) 2))
;
;   We also have to say what do we mean by good-enough. A poor test:
;   (define (good-enough? guess x)
;       (< (abs (- (square guess) x)) 0.001))
;
;   Finally, we need a way to get started. 1 can be used as a good starting guess.
;   (define (sqrt x)
;       (sqrt-iter 1.0 x))
