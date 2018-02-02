(load "stream-library.scm")
(define (integrate-series s)
  (define (iter value series)
    (cons-stream (* (/ 1 value) (stream-car series)) (iter (+ value 1) (stream-cdr series))))
  (iter 1 s))

;;(define (integrate-series s)
;;  (stream-map / s integers))

(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))
