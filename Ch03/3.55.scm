(load "stream-library.scm")
(define (partial-sums s)
    (define (make-partial-sums-stream accumulator stream)
      (cons-stream
        accumulator
        (make-partial-sums-stream (+ accumulator (stream-car stream)) (stream-cdr stream))))
    (make-partial-sums-stream (stream-car s) (stream-cdr s)))


;;better way to do it
(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))
