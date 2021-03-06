(define pi 3.14159265358979)
(define (tan-cf x k)
  (define (iter i ans)
    (if (= i 0)
        (/ x ans)
        (iter (- i 1) (- (- (* i 2) 1) (/ (* x x) ans)))))
  (iter (- k 1) (- (* k 2) 1)))
(tan-cf (/ pi 6) 10)
