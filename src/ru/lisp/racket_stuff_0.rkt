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
;    0 1 0
;   0 1 1 0
;  0 1 2 1 0
; 0 1 3 3 1 0
(define (pascal row pos)
  (cond ((< pos 1) 0)
        ((> pos row) 0)
        ((= row 1) 1)
        (else (+ (pascal (- row 1) pos) (pascal (- row 1) (- pos 1))))
))

; Exercise 1.15
(define (exercise-1-15)
  (define (cube x) (* x x x))
  (define (p x) (and (print 'p) (- (* 3 x) (* 4 (cube x)))))
  (define (sine angle)
    (and (print (list angle))
    (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0))))
    ))
  
  (sine 12.15)
  ; a -> (p) will be called 5 times; 
  ;  12.15 / 3**n < 0.1
  ;  3**n > 121.5
  ;  log-base-3(121.5) = log(121.5) / log(3) = 4.366
  ;  n = 5
  ; b -> n = log(10 * x) / log(3)
)

(define (exercise-1-16)
  (define (expt_ b n)
    (if (= n 0) 1
        (* b (expt_ b (- n 1)))
        ))
  (define (expt_iter b n)
    (define (f n r)
      (if (= n 0) r
          (f (- n 1) (* r b))))
    (f n 1))
  
  (define (square n) (* n n))
  
  (define (fast-expt b n)
    (cond ((= 0 n) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))
  
  (define (fast-expt-it b n)
    (define (f b n r)
      (cond ((= n 0) 1)
            ((= n 1) r)
            ((even? n) (f b (/ n 2) (square r))) ; just squared "r"
            (else (f b (- n 1) (* b r)))))
    (f b n 2))

  ;(print (fast-expt 2 8))
  (print (fast-expt-it 2 32))
)

(define (exercise-1-17)
  (define (mult a b)
    (if (= b 0) 0 (+ a (mult a (- b 1)))))
  
  (define (double n) (* n 2))
  (define (halve n) (/ n 2))
  (define (fast-mult a b)
    (cond ((= b 0) 0)
          ((even? b) (double (fast-mult a (halve b))))
          (else (+ a (fast-mult a (- b 1))))))

  (print (list 
          (mult 11 10) 
          (fast-mult 11 10) 
          (fast-mult 11 1001) 
          ))
)

(define (exercise-1-18) ; will be the same as (fast-expt-it)
  (print 0))

(define (exercise-1-19)
  (print `TODO)
)

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
        
