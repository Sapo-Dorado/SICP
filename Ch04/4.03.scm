;;add a line to each type of evaluaion to put it into a table under the tag of that type
(define (exp-type exp) (car exp))
(define (eval exp env)
  (cond ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((get (type exp)) ((get (type exp)) exp env))
         ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
        (error "Unknown expression type: EVAL" exp))))


;;examples
(put 'define eval-definition)
(put 'lambda (lambda (exp env) (make-procedure (lambda-parameterrs exp)
                                               (lambda-body exp)
                                               env)))

