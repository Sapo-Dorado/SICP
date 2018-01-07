(load "constraint-library.scm")
(define (averager a b c)
  (let ((d (make-connector))
        (e (make-connector)))
    (constant .5 e)
    (adder a b d)
    (multiplier d e c)))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(averager a b c)

(probe "a" a)
(probe "b" b)
(probe "average" c)

(set-value! a 22 'user)
(set-value! b 44 'user)
