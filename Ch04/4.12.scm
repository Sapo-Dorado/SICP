;I hope this works I can't test it yet and i probably won't this was just my attempt at abstracting the procedure

(define (find-var var env local?)
  (define (env-loop env)
    (define (scan frame)
      (let ((var-pair (assoc var frame)))
        (if var-pair
          var-pair
          (if local?
            #f
            (env-loop (enclosing-environment env))))))
    (if (eq? env the-empty-environment)
      #f
      (scan (first-frame env))))
  (env-loop env))
(define (set-variable-value! var val env)
  (let ((var-pair (find-var var env #f)))
    (if var-pair
      (set-cdr! var-pair val)
      (error "unbound variable in environment: SET-VARIABLE-VALUE!"))))
(define (lookup-variable-value var env)
  (let ((var-pair (find-var var env #f)))
    (if var-pair
      (cdr var-pair)
      (error "unbound variable in environment LOOKUP-VARIABLE-VALUE"))))
(define (define-variable-value var val env)
  (let ((var-pair (find-var var env #t)))
    (if var-pair
      (set-cdr! var-pair val)
      (add-binding-to-frame! var val (first-frame env)))))
