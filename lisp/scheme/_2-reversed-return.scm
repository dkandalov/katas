(define (log message)
  (display message)
  (newline)
)

(define (f)
  (+ 100 (call/cc (lambda (return)
    (log 2)
    (set! saved-return return)
    100
  )))
)

(define (main args)
  (define count 0)
  (log 1)
  (log (f))
  (log 4)
  (if (< count 3) (begin
    (set! count (+ 1 count))
    (log "🚀")
    (saved-return count)
  ))
)
