#lang scheme
; the sqrt proceedure with locally defined functions,
; so that they don't collide with other free variables/fns.

; Such nesting of definitions is called BLOCK STRUCTURE.
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x))
       0.001))
  (define (square x)
    (* x x))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (average guess 
             (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

; But there's a better idea lurking here.
; Since x is in the scope of sqrt 2, we don't have to explicitely
; pass it to other internal functions!
; We allow x to be a free variable inside the internal definitions.
; This is called LEXICAL SCOPING.
(define (sqrt2 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x))
            0.001))
  (define (square a)
    (* a a))
  (define (average a b)
    (/ (+ a b) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
