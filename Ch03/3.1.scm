(define (make-accumulator value)
  (lambda (x)
    (begin (set! value (+ value x))
           value)))

(define a (make-accumulator 5))

(a 23)
(a -21)
(a 3)
