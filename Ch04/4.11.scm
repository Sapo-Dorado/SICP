;;you could use assoc to have cleaner code but I forgot about that function
(define (make-frame variables values) (map cons variables values))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (append (list (cons var val))))
(define (check-var frame) (caar frame))
(define (check-val frame (cdar frame)))
(define (first-binding frame) (car frame))
(define (next-binding frame) (cdr frame))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame) (env-loop (enclosing-environment env)))
            ((eq? var (check-var frame)) (check-val frame))
            (else (scan (rest-bindings frame)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan (first-frame env))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame) (env-loop (enclosing-environment env)))
            ((eq? var (check-var frame)) (set-cdr! (first-binding frame) val))
            (else (scan (rest-bindings frame)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (scan (first-frame env))))
  (env-loop env))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame) (add-binding-to-frame! var val frame))
            ((eq? var (check-var frame)) (set-cdr! (check-binding frame) val))
            (else (scan (rest-bindings frame)))))
    (scan (first-frame env))))

