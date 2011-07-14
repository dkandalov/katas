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
; Fibonacci number (iterative)
(define (fib_ n)
  (define (f c prev1 prev2)
    (if (= c n) (+ prev1 prev2)
        (f (+ c 1) (+ prev1 prev2) prev1)))
  
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (f 2 1 1))))

; Exercise 1.11
(define (f1-11-- n)
  (define (f c prev1 prev2 prev3)
    (if (= c n) (+ prev1 (* 2 prev2) (* 3 prev3))
        (f (+ c 1) (+ prev1 (* 2 prev2) (* 3 prev3)) prev1 prev2)))
  
   (cond ((< n 3) n)
         ((= n 3) 4)
         ((= n 4) 11)
         ((= n 5) 25)
         (else (f 6 25 11 4))
))

(define (f1-11-rec n)
  (cond ((< n 3) n)
        (else (+ (f1-11-rec (- n 1))
                 (* (f1-11-rec (- n 2)) 2)
                 (* (f1-11-rec (- n 3)) 3)))
))
;(f1-11-rec 3) ;4
;(f1-11-rec 4) ;11
;(f1-11-rec 5) ;25
;(f1-11-rec 6) ;59
;(f1-11-rec 14) ;60104
;(f1-11-- 22) ;60726899

; Exercise 1.12
(print "TODO 1.12")

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
        
