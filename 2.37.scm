(define nil ())

(define (accumulate op initial sequence) (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
(if (null? (car seqs))
  nil
  (cons (accumulate op init (accumulate (lambda (x y) (cons (car x) y)) nil seqs))
        (accumulate-n op init (accumulate (lambda (x y) (cons (cdr x) y)) nil seqs)))))

(define (dot-product v w)
      (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v) 
  (map (lambda (x) (dot-product x v)) m))

 (define (transpose mat)
   (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x))  m)))

(matrix-*-matrix (list (list 1 2 3) (list 3 4 5) (list 4 5 6)) (list (list 10 11) (list 12 13) (list 14 12)))
