#lang sicp
;chapter 1.2

;counting change
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

;interesting way to make a list
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;exercise 1.11
;A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3
;Regular recursion
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

;iterative
(define (f-2 n)
  (define (f-iter a b c count)
    (cond ((= count 0) a)
          ; we call f(n+1) in terms of the previous a, b, c (assuming that we will start with 2, 1 0)
          ; we start with f(3) since we return without recurison for n<3
          ; f(n+1) in terms of a b c, starting with 2, 1, 0  : a = f(n-1), b = f(n-2), c = f(n-3)
          ; so f(n+1) = a + 2b + 3c which will be our new a (f(n-1)) in the next step
          ; we decrement count. in the next step if count is 0 we return this a
          (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  ;we return  n if n < 3
  (if (< n 3) 
      n
      ;otherwise we call the iterative version starting with a = 2, the first step calculated will be f(3)
      ;we start at count = n - 2 since f(1), f(2) are just b and a. f(0) is c
      (f-iter 2 1 0 (- n 2))))

        
  
  
  