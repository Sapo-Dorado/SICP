(define (named-let? exp) (symbol? (cadr exp)))
(define (named-let-identifier exp) (car exp))
(define (named-let->combination exp)
  (let ((procedure-name (named-let-identifier exp)))
    (make-begin 
     (list
      (list 'define procedure-name 
            (make-lambda 
             (let-vars exp) 
             (let-body exp)))
      (cons procedure-name (let-vals exp))))))

(define (let->combination exp)
  (if (named-let? exp)
    (named-let->combination (cdr exp))
    (cons (make-lambda (let-vars exp) (let-body)) (let-vals exp))))
