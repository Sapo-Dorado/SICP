(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
    (error "Divisor stream cannot have a 0 for the constant term DIV-SERIES:" s2)
    (mul-series s1
                (invert-unit-series s2))))

;;oops its not so easy because i didn't really realize why it was called invert UNIT series until now

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
    (error "Divisor stream cannot have a 0 for the constant term DIV-SERIES:" s2)
    (mul-series s1
                (scale-stream
                  (invert-unit-series
                    (scale-stream (invert-unit-series s2) (/ 1 (stream-car s2)))
                  (stream-car s2))))))

(define tan-series
  (div-series sine-series
              cosine-series))
