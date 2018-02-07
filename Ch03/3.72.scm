(load "stream-library.scm")

(define (square-weight x)
  (+ (square (car x)) (square (cadr x))))

(define square-sum-list (weighted-pairs integers integers square-weight))

(define (triple-squares)
  (define (iter stream)
    (let ((first (stream-car stream))
          (second (stream-ref stream 1))
          (third (stream-ref stream 2)))
      (let ((weight1 (square-weight first))
            (weight2 (square-weight second))
            (weight3 (square-weight third)))
      (if (= weight1 weight2 weight3)
        (cons-stream
          (list weight1 first second third)
          (iter (stream-cdr stream)))
        (iter (stream-cdr stream))))))
  (iter square-sum-list))

(show-stream (triple-squares) 6)

