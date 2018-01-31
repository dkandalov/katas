;;; Examples from http://community.schemewiki.org/?call-with-current-continuation

(define (main args)
  (display "hello\n-----\n\n")
  (example1)
  (example2)
)

(define (example1)
  ;;; Return the first element in LST for which WANTED? returns a true value.
  (define (search wanted? lst)
    (call/cc
      (lambda (return)
        (for-each (lambda (element)
                    (if (wanted? element)
                        (return element)))
                  lst)
        #f)))

  (display "example 1:\n")
  (display (search odd? (list 10 2 3 4)))
  (display "\n-------\n\n")
)

(define (example2)
  ;;; Call TREAT with every element in the LIST and a procedure to call when TREAT likes this element.
  (define (search2 treat lst)
    (call/cc
      (lambda (return)
        (for-each (lambda (element)
                    (treat element return))
                  lst)
        #f)))

  ;;; Call LIKE-IT with a custom argument when we like ELEMENT.
  (define good-element? odd?)
  (define (treat element like-it)
    (if (good-element? element)
        (like-it 'fnord)))

  (display "example 2:\n")
  (display (search2 treat (list 2 3 4)))
  (display "\n-------\n\n")
)
