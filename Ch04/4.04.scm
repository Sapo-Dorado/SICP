;;as syntax procedures
(define (and-exps exp) (cdr exp))
(define (first-and exp) (car exp))
(define (rest-and exp) (cdr exp))
(define (eval-and exp env)
  (let ((expression (and-exps exp)))
    (if (null? exp)
      (error: "null and expression EVAL-AND"))
    (define (iter exp env)
      (cond ((null? exp) #t)
            ((not (true? (eval (first-and exp) env))) #f)
            (else (iter (rest-and exp) env))))
    (iter exp env)))

(define (or-exps exp) (cdr exp))
(define (first-or exp) (car exp))
(define (rest-or exp) (cdr exp))
(define (eval-or exp env)
  (let ((expresion (or-exps exp)))
    (define (iter exp env)
      (cond ((null? exp) #f)
            ((true? (eval (first-or exp))) #t)
            (else (iter (rest-or exp) env))))
    (iter exp env)))

;;as derived expressions
(define (and->if exp)
  (if (null? exp)
    #t
    (make-if (first-and exp)
            (and->if (rest-and exp))
            #f)))
(define (eval-and exp env)
  (let ((expression (and-exps exp)))
    (if (null? expression)
      (error: "null and expression")
      (and->if (and-exps exp) env))))

(define (or->if exp)
  (if (null? exp)
    #f
    (make-if (first-if exp)
              #t
              (or->if (rest-exp)))))

(define (eval-or exp env)
  (or->if exp))

