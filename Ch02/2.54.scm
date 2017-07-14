(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or (not (pair? a)) (not (pair? b))) (eq? a b))
        ((equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (else #f)))

