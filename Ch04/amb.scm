(define *op-table* (make-hash-table))

(define (put type proc)
  (hash-table/put! *op-table* type proc))

(define (get type)
  (hash-table/get *op-table* type #f))

(define (exp-type exp) (car exp))

(define (ambeval exp env succeed fail)
    ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((get (exp-type exp)) ((get (exp-type exp)) exp))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

;;specific-definitions
(define primitive-procedures 
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)))
(define (analyze-self-evaluating exp)
    (lambda (env succeed fail)
        (succeed exp fail)))

(define apply-in-underlying-scheme apply)

(define (tagged-list? exp tag) (if (pair? exp)
            (eq? (car exp) tag)
            false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true) 
        ((boolean? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))
(define (analyze-variable exp)
    (lambda (env succeed fail)
        (succeed (lookup-variable-value exp env) fail)))

(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (make-procedure parameters body env) (list 'procedure parameters body env))

(define (analyze-sequence exps)
    (define (sequentially a b)
        (lambda (env succeed fail) (a env
            (b env succeed fail2))
            fail))
    (define (loop first-proc rest-procs)
        (if (null? rest-procs)
            first-proc
            (loop (sequentially first-proc
                                (car rest-procs))
                                (cdr rest-procs))))
    (let ((procs (map analyze exps)))
        (if (null? procs)
            (error "Empty sequence: ANALYZE"))
            (loop (car procs) (cdr procs))))

(define (analyze-application exp)
    (let ((fproc (analyze (operator exp)))
          (aprocs (map analyze (operands exp))))
        (lambda (env succeed fail)
            (fproc env
                   (lambda (proc fail2)
                    (get-args aprocs
                              env
                              (lambda (args fail3)
                                (execute-application proc args succeed fail3))
                             fail2))
                   fail))))
(define (get-args aprocs env succeed fail)
    (if (null? aprocs)
        (succeed '() fail)
        ((car aprocs) env
                      (lambda (arg fail2)
                        (get-args (cdr aprocs)
                                  env
                                  (lambda (args fail3)
                                    (succeed (cons arg args) fail3))
                                  fail2))
                      fail)))
(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))


;;environment operations
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame vars values) (map cons vars values))
(define (frame-binding var frame) (assoc var frame))
(define (check-var binding) (car binding))
(define (check-val binding) (cdr binding))
(define bound? frame-binding) 
(define (add-binding-to-frame! var val frame)
  (define (add-binding! binding frame)
    (cond ((null? (cdr frame)) (set-cdr! frame binding))
          (else (add-binding! binding (cdr frame)))))
  (add-binding! (list (cons var val)) frame))
(define (set-binding-in-frame! var val frame)
  (set-cdr! (frame-binding var frame) val))

 (define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (if (bound? var frame)
              (let ((val (check-val (frame-binding var frame))))
                (if (eq? val '*unassigned*)
                    (error "variable unassigned:" var)
                    val))
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (if (bound? var frame)
              (set-binding-in-frame! var val frame)
              (env-loop (enclosing-environment env))))))    
  (env-loop env))
 
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (if (bound? var frame)
        (set-binding-in-frame! var val frame)
        (add-binding-to-frame! var val frame))))

;;table-expressions
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))
(define (one-of l) (list-ref l (random (length l))))
(define (remove item l)
  (cond ((null? l) '())
        ((equal? item (car l)) (cdr l))
        (else (cons (car l) (remove item (cdr l))))))
(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((choice (one-of choices)))
              (choice env
                        succeed
                        (lambda () (try-next (remove choice choices)))))))
      (try-next cprocs))))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))
(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices) (fail)
             ((car choices) env
                            succeed
                            (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))


(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (analyze-quoted exp)
    (let ((qval (text-of-quotation exp)))
        (lambda (env succeed fail)
            (succeed qval fail))))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (assignment? exp) (tagged-list? exp 'set!))

(define (analyze-assignment exp)
    (let ((var (assignment-variable exp))
         (vproc (analyze (assignment-value exp))))
        (lambda (env succeed fail)
            (vproc env
                   (lambda (val fail2)
                        (let ((old-value (lookup-variable-value var env)))
                            (set-variable-value! var val env)
                            (succeed 'ok
                                (lambda ()
                                    (set-variable-value! var old-value env)
                                    (fail2)))))
                   fail))))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp)) (caddr exp)
    (make-lambda (cdadr exp)
                  (cddr exp))))

(define (analyze-definition exp)
    (let ((var (definition-variable exp))
          (vproc (analyze (definition-value exp))))
            (lambda (env succeed fail)
                (vproc env
                       (lambda (val fail2) (define-variable! var val env)
                       (succeed 'ok fail2))
                       fail))))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

  (define (analyze-if exp)
    (let ((pproc (analyze (if-predicate exp)))
          (cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternative exp))))
            (lambda (env succeed fail)
                (pproc env
                       (lambda (pred-value fail2)
                            (if (true? pred-value)
                                (cproc env succeed fail2)
                                (aproc env succeed fail2)))
                        fail))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))
(define (analyze-lambda exp)
    (let ((vars (lambda-parameters exp))
         (bproc (analyze-sequence (lambda-body exp))))
            (lambda (env succeed fail)
                (succeed (make-procedure vars bproc env) fail))))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))


(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-recipient clause) (caddr clause))
(define (cond-recipient-clause? clause) (eq? (cadr clause) '=>))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (make-cond-recipient clause predicate)
  (list (cond-recipient clause) predicate))
(define (cond-consequent clause predicate)
  (if (cond-recipient-clause? clause)
      (make-cond-recipient clause predicate)
      (sequence->exp (cond-actions clause))))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses)) (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last: COND->IF" clauses))
          (make-if (cond-predicate first)
                    (cond-consequent (cond-actions first))
                    (expand-clauses rest))))))

(define (let? exp) (tagged-list? exp 'let))
(define (named-let? exp)
  (and (tagged-list? exp 'let))
  (symbol? (cadr exp)))
(define (let-assignments exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-vars exp) (map car (let-assignments exp)))
(define (let-vals exp) (map cadr (let-assignments exp)))
(define (make-let parameters body)
  (cons 'let (cons parameters body)))
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
    (cons (make-lambda (let-vars exp) (let-body exp)) (let-vals exp))))

(put 'quote analyze-quoted)
(put 'set! analyze-assignment)
(put 'define analyze-definition)
(put 'if analyze-if)
(put 'lambda analyze-lambda)
(put 'begin (lambda (exp) (analyze-sequence (begin-actions exp))))
(put 'cond (lambda (exp) (analyze (cond->if exp))))
(put 'let (lambda (exp) (analyze (let->combination exp))))
(put 'amb analyze-amb)
(put 'ramb analyze-ramb)

;;global environment
(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects) (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin (newline)
                 (display ";;; Starting a new problem ")
                 (ambeval input
                          the-global-environment
                          (lambda (val next-alternative)
                            (announce-output output-prompt)
                            (user-print val)
                            (internal-loop next-alternative))
                          (lambda ()
                            (announce-output ";;; There are no more values of")
                            (user-print input)
                            (driver-loop)))))))
  (internal-loop (lambda ()
                   (newline)
                   (display ";;; There is no current problem")
                   (driver-loop))))


(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))
(driver-loop)
