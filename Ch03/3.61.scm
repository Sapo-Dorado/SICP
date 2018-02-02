(define (invert-unit-series s)
  (stream-cons 1
               (scale-stream (mul-streams (stream-cdr s)
                                          (invert-unit-series s))
                             -1)))

