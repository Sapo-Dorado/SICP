(define (smooth-stream input)
  (define (iter stream last-value last-avpt)
    (let ((avpt (/ (+ (stream-car stream)
                      last-value)
                   2)))
      (cons-stream avpt
                   (iter (stream-cdr stream) (stream-car stream) avpt))))
  (iter input 0 0))

(define (make-zero-crossings input)
  (let ((smooth (smooth-stream input)))
    (define (iter stream last-value)
      (cons-stream (signal-change-detector (stream-car stream) last-value)
                   (iter (stream-cdr stream) (stream-car stream))))
    (iter smooth 0)))
;; with stream-map

(define (make-zero-crossings input)
  (define (smooth-stream input)
    (stream-map (lambda (x y) (/ (+ x y) 2)) input))
  (let ((smooth (stream-map smooth-stream input)))
    (stream-map signal-change-detector smooth (stream-cons 0 smooth))))
