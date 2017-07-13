(define (cont-frac-iter n d k)
  (define (iter i answer)
    (if (= i 0)
      answer
      (iter (- i 1) (/ (n i) (+ (d i) answer)))))
  (iter (- k 1) (/ (n k) (d k))))

(define (cont-frac n d k)
  (define (frac i)
    (if (> i k)
        (d i)
        (+ (d i) (/ (n (+ i 1)) (frac (+ i 1))))))
  (/ (n 1) (frac 1)))
