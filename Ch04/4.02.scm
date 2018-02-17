;; application would get triggered by a definition which would result in an error
;; we can fix this by implementing a call tag

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
