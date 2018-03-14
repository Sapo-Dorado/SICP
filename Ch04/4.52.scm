(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-first-exp exp) (cadr exp))
(define (if-fail-fail-exp exp) (caddr exp))
(define (analyze-if-fail exp)
  (let ((first (analyze (if-fail-first-exp exp)))
        (alt (analyze (if-fail-fail-exp exp))))
    (lambda (env succeed fail)
      (first env
             succeed
             (lambda ()
               (alt env succeed fail))))))



