(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (left-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (cons (first)
              (left-list-of-values (rest-operands exps) env)))))

(define (right-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (right-list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              (rest)))))

