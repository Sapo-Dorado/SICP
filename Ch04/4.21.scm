;;fib implementation
((lambda (n)
   ((lambda (fib) (fib fib 0 1 n))
    (lambda (fb a b n) (if (= n 0) a (fb fb b (+ a b) (- n 1))))))
 10)

;;even-odd implementation
(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 12)
(f 121)
