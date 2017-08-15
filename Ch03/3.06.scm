(define (rand)
  (let ((x rand-init))
    (lambda (message)
      (cond ((eq? message 'generate) (set! x (rand-update x)) x)
            ((eq? message 'reset) (set! x rand-init) x)
            (else (error "Invalid argument RAND:" message))))))

(define rand-init 2)
(define (rand-update num)
  (- (* num 2) 1))

(define a (rand))

(a 'generate)
(a 'generate)
(a 'generate)
(a 'reset)
(a 'generate)
