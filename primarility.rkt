#lang scheme

;; Checking for Prime Numbers
;; One way to check if the number is prime is to find the number's divisors.
;; The following program finds the smallest integral divisor (greater than 1) of
;; a given number n. It does it in a straightforward way, by testing n for divisibility
;; by successive integers starting with 2.

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
   ((> (square test-divisor) n) n)
   ((divides? test-divisor n) test-divisor)
   (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

;; n is prime if and only if n is its own smallest divisor
(define (prime? n)
  (= n (smallest-divisor n)))

;; The end test for find-divisor is based on the fact that if n is not prime
;; it must have a divisor les than or equal to square-root of n. This means that
;; algorithm need only test divisors between 1 and square-root(n). Consequently
;; the number of steps required to identify n as prime will have order of growth
;; Theta(square-root of n)
