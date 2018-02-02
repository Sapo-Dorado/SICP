(define (ln-of-2 tolerance)
  (define (ln-summands n)
    (cons-stream (/ 1.0 n)
                 (stream-map - (ln-summands (+ n 1)))))
  (define guesses 
    (partial-sums (ln-summands 1)))
  (stream-limit guesses tolerance))
