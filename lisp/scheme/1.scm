(load "common")

(define events (list))
(define (log event)
  (set! events (append events (list event)))
)

(define (f arg k)
  (log 2)
  (set! k (call/cc (lambda (mk) (k mk))))
  (log 4)
  (log arg) ;; "arg" keeps its value after continuation
  (k #f)
)

(define (main args)
  (log 1)
  (define fk #t)
  (set! fk (call/cc (lambda (k) (f "f-arg" k))))
  (log 3)
  (call/cc (lambda (k) (fk k)))
  (log 5)

  (expect-to-be-equal events (list 1 2 3 4 "f-arg" 5))
)
