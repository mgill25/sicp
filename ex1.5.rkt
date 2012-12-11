#!/usr/bin/env racket
#lang racket
; SICP question 1.5
; Determine whether the interpreter is using application-order-evaluation, or normal-order evaluation.
; Ben Bitdiddle defins the following two procedures: 

(define (p) (p))

(define (test x y)
    (if (= x 0) 0 y))

; then he evaluates the expression.
(test 0 (p))
