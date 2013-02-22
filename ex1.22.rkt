#lang scheme

;; Most Lisp implementations include a primitive called runtime that returns an
;; integer that specifies the amount of time the system has been running (measured,
;; for example, in microseconds). The following timed-prime-test procedure, when
;; called with an integer n, prints n and checks to see if n is prime. If n is
;; prime, the procedure prints three asterisks followed by the amount of time
;; used in performing the test.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((prime? n)
	 (report-prime (- (current-inexact-milliseconds) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

;; Using this procedure, write a procedure search-for-primes that checks the primality
;; of consecutive odd integers in a specified range. Use your procedure to find the
;; three smallest primes larger than 1000, 10000, 100000. Note the time needed to test
;; each prime. Since the testing algorithm has order of growth O(square-root n), you should
;; expect that testing for primes around 10,000 should take about Theta(root 10) times as long
;; as testing for primes around 1000. Do your timing data bear this out? How well do the data
;; for 100,000 and 1,000,000 support the (square-root n) prediction? Is your result compatible
;; with the notion that programs on your machine run in time proportional to the number of steps
;; required for the computation?

;; Checking for Primarility
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
	[(divides? test-divisor n) test-divisor]
	[else (find-divisor n (+ test-divisor 1))]))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n) (* n n))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes start end) ;; Remember, only checks consecutive odd integers.
  (if (even? start)
      (search-for-primes (+ start 1) end)
      (cond ((< start end) (timed-prime-test start)
	     (search-for-primes (+ start 2) end)))))

;; Time taken to find the first 3 primes larger than 1000:
;; 1009 *** 0.032958984375
;; 1013 *** 0.03515625
;; 1019 *** 0.032958984375

;; Time taken to find the first 3 primes larger than 10000:
;; 10007 *** 0.10498046875
;; 10009 *** 0.10498046875
;; 10037 *** 0.10400390625

;; Time taken to find the first 3 primes larger than 100000:
;; 100003 *** 0.328857421875
;; 100019 *** 0.328125
;; 100043 *** 0.328125

;; Square root of 10: approx. 3.1622
;; Testing, the assertion that time taken to compute primes around 10,000 take about
;; (sqrt 10) times as long as those around 1,000, and similarly, time taken to
;; compute primes around 100,000 takes around (sqrt 10) times as long as those
;; around 10,000. Here, sqrt is Newton's method of calculating square roots.
