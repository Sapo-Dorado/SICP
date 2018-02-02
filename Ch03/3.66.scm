(load "stream-library.scm")

(define a (pairs integers integers))

(define (find-pair x y s)
  (define (iter stream count)
    (let ((x-value (car (stream-car stream)))
          (y-value (cadr (stream-car stream))))
      (if (and (= x-value x) (= y-value y))
        count
        (iter (stream-cdr stream) (+ count 1)))))
  (iter s 0))


