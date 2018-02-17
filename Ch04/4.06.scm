(define (let-assignments exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-vars exp) (map car (let-assignments exp)))
(define (let-vals exp) (map cadr (let-assignments exp)))

(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body)) (let-vals exp)))



