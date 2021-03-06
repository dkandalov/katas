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
    (jump-out #f)
  ))
)

(define (main args)
  (define count 0)
  (log 1)
  (f)
  (resume)
  (log 3)
  (if (< count 3) (begin
    (set! count (+ 1 count))
    (log "🚀")
    (resume)
  ))
)
