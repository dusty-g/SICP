#lang sicp

(define (square x) (* x x))

(define (sum-of-squares x y ) (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;1.1.6
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

; (if <predicate> <consequent> <alternative>)

(define (alt-abs x)
  (if (< x 0)
      (- x)
      x))

; (and <e1> <e2> ... <en>), (or ...), (not <e>)
(define (five-to-ten x)
  (and (> x 5)
       (< x 10)))
;1.3
(define (sum-of-larger-squares x y z)
  (cond ((and (>= x y) (>= z y)) (sum-of-squares x z))
        ((and (>= x z) (>= y z)) (sum-of-squares x y))
        ((and (>= z x) (>= y x)) (sum-of-squares y z))))

;1.1.7
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

