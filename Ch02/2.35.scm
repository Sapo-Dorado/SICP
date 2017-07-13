(define nil ())
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate (lambda (x y) 
                       (if (pair? x)
                          (+ (count-leaves x) y)
                          (+ y 1))
                       ) 0 t))

(count-leaves (list (list 1 2 3)(list 2 3 4)))
