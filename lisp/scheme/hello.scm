;;; Examples from http://community.schemewiki.org/?call-with-current-continuation

(define (main args)
  (display "hello\n-----\n\n")
  (example1)
  (example2)
  (example3)
  (example4)
)

(define (example1)
  ;;; Return the first element in "lst" for which "wanted?" returns a true value.
  (define (search wanted? lst)
    (call/cc (lambda (return)
      (for-each
        (lambda (element) (if (wanted? element) (return element)))
        lst)
    #f)))

  (display "example 1:\n")
  (display (search odd? (list 10 2 3 4)))
  (display "\n-------\n\n")
)

(define (example2)
  ;;; Call TREAT with every element in the "lst" and a procedure to call when "treat" likes this element.
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

(define (example3)
  (define return #f)
  (define return-count 0)

  (display "example 3:\n")
  (display (+ 1 (call/cc
         (lambda (cont)
           (set! return cont)
           1))))
  (cond ((< return-count 3) (
    (set! return-count (+ 1 return-count))
    (return 22)
  )))
  (display "\n-------\n\n")
)

(define (example4)
  (define (hefty-computation do-other-stuff)
   (let loop ((n 5))
     (display "Hefty computation: ")
     (display n)
     (newline)
     (set! do-other-stuff (call/cc do-other-stuff))
     (display "Hefty computation (b)\n")
     (set! do-other-stuff (call/cc do-other-stuff))
     (display "Hefty computation (c)\n")
     (set! do-other-stuff (call/cc do-other-stuff))
     (if (> n 0)
         (loop (- n 1)))))

  (define (superfluous-computation do-other-stuff)
    (let loop ()
      (for-each (lambda (it)
                  (display it)
                  (newline)
                  (set! do-other-stuff (call/cc do-other-stuff)))
                '("1" "2" "3" "4"))
      (loop)))

  (hefty-computation superfluous-computation)
)
