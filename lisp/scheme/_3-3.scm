(define (log message)
  (display message)
  (newline)
)

(define (make-generator callback)
  (define (yield)
    (call/cc (lambda (continuation)
      (set! yield-point continuation)
      (jump-out #f)
  )))
  (define (resume)
    (call/cc (lambda (continuation)
      (set! jump-out continuation)
      (yield-point #f)
  )))
  (define yield-point #f)
  (set! yield-point (lambda (_)
    (callback yield)
    (jump-out #f)
  ))
  resume
)

(define (f yield)
  (log 2)
  (yield)
  (log 4)
  (yield)
  (log 6)
)

(define (main args)
  (log 1)
  (define resume (make-generator f))
  (resume)
  (log 3)
  (resume)
  (log 5)
  (resume)
  (log 7)
)
