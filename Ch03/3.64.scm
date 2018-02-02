(define (stream-limit stream tolerance)
  (let ((v1 (stream-car stream))
        (v2 (stream-ref stream 1)))
    (if (< (abs (- v1 v2)) tolerance)
      v2
      (stream-limit (stream-cdr stream) tolerance))))

