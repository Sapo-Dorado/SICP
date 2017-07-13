(define (print-point p) (newline)
  (display "(") (display (x-point p)) (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (make-point x y)
  (cons x y))

(define (midpoint segment)
  (let ((x1 (car (car segment)))
        (y1 (cdr (car segment)))
        (x2 (car (cdr segment)))
        (y2 (cdr (cdr segment))))
        (make-point (average x1 x2) (average y1 y2))))

(define (average a b)
  (let((average (/ (abs (- a b)) 2)))
       (cond ((< a b) (+ a average))
             (else (+ b average)))))

(define line (make-segment (make-point 0 6) (make-point 8 0)))
(midpoint line)
