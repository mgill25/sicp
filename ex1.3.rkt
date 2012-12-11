#!/usr/bin/env racket
#lang racket

; Define a procedure that takes 3 numbers as arguments and returns
; the sum of the squares of the two larger numbers.

(define (max a b)
  (if (> a b) a b))

; Check the greatest of three numbers.
(define (bigger a b c)
  (define result (max a b))
  (if (> result c) result c))

;----------------------------------------------------------------------------
; In fact, we don't need to do all this work. We can just ignore the smallest

(define (min a b)
   (if (< a b) a b))
; smallest
(define (smallest a b c)
 (define result (min a b))
 ( if (< result c) result c))

; sum-of-squares
(define (sum-of-squares a b)
  (+ (* a a) (* b b)))

; Answer. Sum of squares of two larger numbers.
(define (square-largest-two a b c)
  (define ignore (smallest a b c))
  (cond ((= ignore a) (sum-of-squares b c))
        ((= ignore b) (sum-of-squares a c))
        ((= ignore c) (sum-of-squares a b))))
;----------------------------------------------------------------------------
; An easier way. Since we already have the max and min methods.
(define (square a)
  (* a a))

(define (square-largest-two-2 a b c)
  (+ (square (max a b))
     (square (max (min a b) c))))


