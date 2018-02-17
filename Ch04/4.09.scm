;while is used for while statements this while loop checks the predicate then runs the body of the procedure if true and keeps looping until false
(define (while-predicate exp) (car exp))
(define (while-body exp) (cdr exp))
(define (while-eval exp env)
  (if (true? (while-predicate exp))
      (begin (eval (while-body exp) env)
             (while-eval exp env))
           'done))


