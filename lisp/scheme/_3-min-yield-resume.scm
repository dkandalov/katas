(define (log event)
  (display event)
  (newline)
)

(define (make-generator f)
  (define (yield)
    (call/cc (lambda (continuation)
      (set! run-procedure (lambda () (continuation #f)))
      (jump-out)
  )))

  (define (resume)
    (call/cc (lambda (continuation)
      (set! jump-out (lambda () (continuation #f)))
      (run-procedure)
  )))

  (set! run-procedure (lambda ()
    (f yield)
    (jump-out)
  ))
  resume
)

(define (f yield)
  (log 2)
  (yield)
  (log 4)
)

(define (main args)
  (define next (make-generator f))
  (log 1)
  (next)
  (log 3)
  (next)
  (log 5)
  (next)
  (log "xxx")
  (log "🚀")
  (next)
  (log "🚀")
  (next)
)
