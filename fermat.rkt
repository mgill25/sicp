#lang scheme

;; The Fermat Test
;; The Theta(log n) primality test is based on a result from number theory
;; known as Fermat's Little Theorem.
;; Fermat's Little Theorem: If n is a prime number and a is any positive integer
;; less than n, then a raised to the n-th power is congruent to a modulo n.
;; (Two numbers are said to be `congruent modulo` n if they both have the
;; same remainder when divided by n. The remainder of a number a when divided
;; by n is also referred to as the remainder of a modulo n, or simply as
;; a modulo n.)

;; Algorithm: Given a number n, pick a random number a < n, and compute
;; the remainder of a raised to the power n modulo n. If result is not
;; equal to a, then n is certainly not a prime.If it is a, then chances
;; are good that n is a prime. Now pick another random a, and test with
;; the same method. If it also satisfies the equation, then we can be
;; even more certain that n is prime. Try more values of a to increase
;; confidence in the result. This is known as Fermat's Test.

;; Proceedure that computes the exponential of a number modulo another number
(define (expmod base exp m)
  (cond
   ((= exp 0) 1)
   ((even? exp)
    (remainder (square (expmod base (/ exp 2) m))
               m))
   (else
    (remainder (* base (expmod base (- exp 1) m))
               m))))
;; Uses successive squaring, so number of steps grows logarithmically with the exponent.
(define (square x) (* x x))

;; The Fermat's test is performed by choosing a random number a b/w 1 and (n-1) inclusive and
;; checking whether the remainder modulo n of the nth-power of a is equal to a. The random number
;; a is chosen using the proceedure `random`, which we assume is included as a primitive in
;; Scheme. Random returns a nonnegative integer less than its integer input.

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; Run the test for a given number of times.
(define (fast-prime? n times)
  (cond
   ((= times 0) #t)
   ((fermat-test n) (fast-prime? n (- times 1)))
   (else #f)))
