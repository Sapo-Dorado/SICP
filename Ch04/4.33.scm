(define (eval-quoted exp env)
  (text-of-quotation exp env))
(define (quotation->cons exp)
  (if (null? exp)
      exp
      (list 'cons (list 'quote (car exp)) (quotation->cons (cdr exp)))))
(define (text-of-quotation exp env)
  (let ((result (cadr exp)))
    (if (pair? result)
        (eval (quotation->cons result) env)
        result)))
