#lang scheme
;; Greatest Common Divisor
;; GCD of two integers a and b is defined as the largest integer that divides both
;; a and b with no remainder. To reduce a rational number to lowest terms, we must
;; divide both the numerator and denominator by their GCD. One way to compute GCD of
;; two numbers is to factor them and search for common factors, but there's a famous
;; algorithm that is much more efficient. 

;; Euclid's Algorithm:
;; The idea is that if `r` is the remainder when a is divided by b, then
;; the common divisors of a and b are preciously the same as the common divisors of
;; b and r. Thus, we can use the equation:
;; GCD(a, b) = GCD(b, r)
;; to successively reduce the problem of computing a GCD to the problem of smaller
;; and smaller pair of integers.

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; This generates an iterative process, whose number of steps grows
;; as the log of the numbers involved.
