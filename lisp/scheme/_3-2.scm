(define (log message)
  (display message)
  (newline)
)

(define (f)
  (set! yield (lambda ()
    (call/cc (lambda (continuation)
      (set! yield-point continuation)
      (jump-out #f)
  ))))
  (set! resume (lambda ()
    (call/cc (lambda (continuation)
      (set! jump-out continuation)
      (yield-point #f)
  ))))
  (set! yield-point (lambda (_)
    (log 2)
    (yield)
    (log 4)
    (yield)
    (log 6)
    (jump-out #f)
  ))
)

(define (main args)
  (log 1)
  (f)
  (resume)
  (log 3)
  (resume)
  (log 5)
  (resume)
  (log 7)
)