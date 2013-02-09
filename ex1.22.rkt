#lang scheme

;; Most Lisp implementations include a primitive called runtime that returns and
;; integer that specifies the amount of time the system has been running (measured,
;; for example, in microseconds). The following timed-prime-test procedure, when
;; called with an integer n, prints n and checks to see if n is prime. If n is
;; prime, the procedure prints three asterisks followed by the amount of time
;; used in performing the test.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; Using this procedure, write a procedure search-for-primes that checks the primality
;; of consecutive odd integers in a specified range. Use your procedure to find the
;; three smallest primes larger than 1000, 10000, 100000. Note the time needed to test
;; each prime. Since the testing algorithm has order of grown O(square-root n), you should
;; expect that testing for primes around 10,000 should take about Theta(root 10) times as long
;; as testing for primes around 1000. Do your timing data bear this out? How well do the data
;; for 100,000 and 1,000,000 support the (square-root n) prediction? Is your result compatible
;; with the notion that programs on your machine run in time proportional to the number of steps
;; required for the computation?


;; We can use Fermat's test to write a basic procedure to check for primes, as a helper function. 
(define (expmod base exp m)
  (cond
   ((= exp 0) 1)
   ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                           m))
   (else
    (remainder (* base (expmod base (- exp 1) m))
               m))))

(define (fermat-test n)
  (define try-it (lambda (a)
                   (= (expmod a n n) a)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
   ((= times 0) #t)
   ((fermat-test n) (fast-prime? n (- times 1)))
   (else #f)))

;;(define (search-for-primes lower upper)  )

