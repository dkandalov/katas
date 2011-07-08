(define (assert-true value)
  (if (value) (= 1 1) (error "AAA")))
(define (assert-true value)
  (if (value) (= 1 1) (error "AAA")))

; Ackermann's function
(define (ackermann x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(define (ackermann_f1 n) (A 0 n))
(define (ackermann_f2 n) (A 1 n))
(define (ackermann_f3 n) (A 2 n))

; Fibonacci number (with tree recursion)
(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) 
                 (fib (- n 2))))
        )
  )

; Exercise 1.11
(define (f1_11 n)
  (cond ((< n 3) n)
        (else (+ (f1_11 (- n 1))
                 (f1_11 (- n 2))
                 (f1_11 (- n 3))))))


; Exercise 2.20
(define (odd n) ; used this because (= #t #t) doesn't work
  (if (< n 2) n (odd (- n 2))))
(define (same-parity n . a_list)
  (define (filter l)
    (cond ((null? l) l)
          ((= (odd n) (odd (car l))) (cons (car l) (filter (cdr l))))
          (else (filter (cdr l)))))
  (cons n (filter a_list)))

;(assert-true (lambda () ((list= eq? (same-parity 1 2 3 4 7 12) (list 1 3 7))))) TODO
        
