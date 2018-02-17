(load "stream-library.scm")
;; random-numbers doesn't work because I don't have random commands but assuming random-numbers creates a stream of random numbers it should work because when you go back to  the beginning of the stream the stream should have the same numbers because the stream is memoized
(define null '())
(define (rand input)
  (define random-numbers
    (cons-stream (random init)
                 (stream-map rand-update random-numbers)))
  (define (return-stream commands nums)
    (cond ((stream-null? commands) null)
          ((eq? (stream-car commands) 'generate)
           (cons-stream (stream-car nums)
                        (return-stream (stream-cdr commands) (stream-cdr nums))))
          ((eq? (stream-car commands) 'reset)
           (return-stream (stream-cdr commands) random-numbers))
          (else (error "invalid command: ") (stream-car commands))))
  (return-stream input (stream-cdr commands)))