;;I really dont know how to do random numbers so im just going to pretend i've defined random-in-range that produces a stream of random numbers in a given range
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
       (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (monte-carlo-integration P x1 x2 y1 y2)
  (define test-stream
    (stream-map cons (random-in-range x1 x2) (random-in-range y1 y2)))
  (define experiment-stream
    (stream-map P test-stream)) 
  (let ((area (* (- x2 x1) (- y2 y1))))
    (scale-stream (monte-carlo experiment-stream 0 0) area)))
  
