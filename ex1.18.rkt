#lang scheme
;; Exercise 1.18
;; Using the results of Exercise 1.16 and 1.17, devise a procedure
;; that generates an iterative process for multiplying two integers
;; in terms of adding, doubling, and halving and uses a logarithmic
;; number of steps.

;; An iterative process for multiplying two integers.

(define (double x) (* x 2))

(define (halve x)
  (cond
    ((even? x) (/ x 2))))

(define (fast-mult a b)
  (iter-mult2 a b 0))         ; We start with n = 0

;; When b is odd (and not 1), we replace n with the current
;; value of a. When b = 1, the current value of n is added to
;; that of a, returning the final result.

;; EDIT: Seems like iter-mult is buggy, and producing incorrect
;; results for large values of a and b.
;; I'm still keeping it here for future reference.
(define (iter-mult a b n)               ;; NOT CORRECT!
  (cond
    ((= b 0) 0)
    ((= b 1) (+ a n))
    ((even? b) (iter-mult (double a) (halve b) n))
    (else (iter-mult a (- b 1) a))))

;; Sample Run   
; (2 5 0)
; (2 4 2)
; (4 2 2)
; (8 1 2)
; (8 0 10)

;; A Simpler Version.
;; When b is odd, n = a + n
(define (iter-mult2 a b n)
  (cond
    ((= b 0) n)
    ((even? b) (iter-mult2 (double a) (halve b) n))
    (else (iter-mult2 a (- b 1) (+ a n)))))
