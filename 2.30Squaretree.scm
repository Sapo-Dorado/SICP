(define (square-tree x)
  (cond ((null? x) ()
        ((not (pair? x)) (* x x))
        (else (cons (square-tree (car x)) (square-tree (cdr x)))))))

(define (mapped-square-tree y)
  (map (lambda (x)
         (cond ((pair? x) (mapped-square-tree x))
               (else (* x x)))) y))

(define nil ())

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
           (append rest (map (lambda (x)
                             (cons (car s) x)) 
                             rest)))))

(subsets (list 1 2 3))
