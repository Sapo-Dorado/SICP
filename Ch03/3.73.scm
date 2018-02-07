(load "stream-library.scm")
(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C))
                           v0
                           dt))))

(define RC1 (RC 5 1 0.5))
(show-stream (RC1 integers .1) 5)
