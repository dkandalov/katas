;; Based on https://stackoverflow.com/questions/44514890/does-call-cc-in-scheme-the-same-thing-with-yield-in-python-and-javascript#44522334

(define (make-generator procedure)
  (define last-return #f)
  (define (last-continuation)
    ((lambda (value) (last-return value)) (procedure yield))
  )

  (define (yield value)
    (call/cc (lambda (continuation)
      (set! last-continuation (lambda () (continuation #f)))
      (last-return value)
  )))

  (define (resume)
    (call/cc (lambda (return)
      (set! last-return return)
      (last-continuation)
  )))
  resume
)

(define (main args)
  (define test (make-generator (lambda (yield)
      (yield 1)
      (yield 2)
      (yield 3)
  )))
  (display (test))
  (display (test))
  (display (test))
  (display (test))
  (display (test))
)
