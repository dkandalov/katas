; current-continuation : -> continuation
(define (current-continuation)
 (call-with-current-continuation
  (lambda (cc)
   (cc cc))))

; fail-stack : list[continuation]
(define fail-stack '())

; fail : -> ...
(define (fail)
 (if (not (pair? fail-stack))
  (error "back-tracking stack exhausted!")
  (begin
   (let ((back-track-point (car fail-stack)))
	(set! fail-stack (cdr fail-stack))
	(back-track-point back-track-point))
  )))

; amb : list[a] -> a
(define (amb choices)
 (let ((cc (current-continuation)))
  (cond
   ((null? choices)      (fail))
   ((pair? choices)      (let ((choice (car choices)))
						  (set! choices (cdr choices))
						  (set! fail-stack (cons cc fail-stack))
						  choice))
  )))

; (assert_ condition) will cause
; condition to be true, and if there
; is no way to make it true, then
; it signals and error in the program.
(define (assert_ condition)
 (if (not condition)
  (fail)
  #t))


; The following prints (4 3 5)
(let ((a (amb (list 1 2 3 4 5 6 7)))
	  (b (amb (list 1 2 3 4 5 6 7)))
	  (c (amb (list 1 2 3 4 5 6 7))))

 ; We're looking for dimensions of a legal right
 ; triangle using the Pythagorean theorem:
 (assert_ (= (* c c) (+ (* a a) (* b b))))

 (display (list a b c))
 (newline)

 ; And, we want the second side to be the shorter one:
 (assert_ (< b a))

 ; Print out the answer:
 (display (list a b c))
 (newline))






;; SAT-solving with amb.

(define (implies a b) (or (not a) b))

;; The is not the most efficient implementation,
;; because a continuation is captured for each
;; occurrence of the same variable, instead of
;; one for each variable.
(define-syntax sat-solve
 (syntax-rules (and or implies not)
  ((_ formula body)
   ; =>
   (sat-solve formula body formula))

  ((_ (not phi) body assertion)
   ; =>
   (sat-solve phi body assertion))

  ((_ (and phi) body assertion)
   ; =>
   (sat-solve phi body assertion))

  ((_ (and phi1 phi2 ...) body assertion)
   ; =>
   (sat-solve phi1 (sat-solve (and phi2 ...) body assertion)))

  ((_ (or phi) body assertion)
   ; =>
   (sat-solve phi body assertion))

  ((_ (or phi1 phi2 ...) body assertion)
   ; =>
   (sat-solve phi1 (sat-solve (or phi2 ...) body assertion)))

  ((_ (implies phi1 phi2) body assertion)
   ; =>
   (sat-solve phi1 (sat-solve phi2 body assertion)))

  ((_ #t body assertion)
   ; =>
   body)

  ((_ #f body assertion)
   ; =>
   (fail))

  ((_ v body assertion)
   (let ((v (amb (list #t #f))))
	(if (not assertion)
	 (fail)
	 body)))))


; The following prints (#f #f #t)
(display
 (sat-solve (and (implies a (not b))
			 (not a)
			 c)
  (list a b c)))