(define *op-table* (make-hash-table))

(define (put type1 type2 proc)
  (hash-table/put! *op-table* (list type1 type2) proc))

(define (get type1 type2)
  (hash-table/get *op-table* (list type1 type2) #f))

;;qeval section
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame) (stream-append-delayed (find-assertions query-pattern frame)
                                           (delay (apply-rules query-pattern frame))))
    frame-stream))

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum) (check-an-assertion datum pattern frame)) (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))


(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat)) (pattern-match (cdr pat)
                                                      (cdr dat)
                                                      (pattern-match (car pat) (car dat) frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp) (make-new-variable exp rule-application-id))
            ((pair? exp) (cons (tree-walk (car exp))
                               (tree-walk (cdr exp))))
            (else exp))) (tree-walk rule)))


(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2)) (unify-match (cdr p1)
                                                  (cdr p2)
                                                  (unify-match (car p1)
                                                               (car p2)
                                                               frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding (unify-match (binding-value binding) val frame))
          ((var? val) (let ((binding (binding-in-frame val frame)))
                        (if binding
                            (unify-match var
                                         (binding-value binding)
                                         frame)
                            (extend var val frame))))
          ((depends-on? val var frame) 'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e) (if (equal? var e)
                        true
                        (let ((b (binding-in-frame e frame)))
                          (if b
                              (tree-walk (binding-value b))
                              false))))
      ((pair? e) (or (tree-walk (car e))
                     (tree-walk (cdr e))))
      (else false)))
  (tree-walk exp))

;;assertion-table functions
(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
  (if (use-index? pattern) (get-indexed-rules pattern) (get-all-rules)))
(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
  (stream-append (get-stream (index-key-of pattern) 'rule-stream)
                 (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS (cons-stream assertion old-assertions))
    'ok))
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules)) 'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
                (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat) (constant-symbol? (car pat)))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream (stream-car s1)
                   (stream-append-delayed (stream-cdr s1)
                                          delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream (stream-car s1)
                   (interleave-delayed (force delayed-s2)
                                       (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed (stream-car stream)
                          (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x) (cons-stream x the-empty-stream))

;;Query helper procs
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))
(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp) (eq? (type exp) 'assert!))
(define (add-assertion-body exp) (car (contents exp)))
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement) (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule)) '(always-true) (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp) (cons (map-over-symbols proc (car exp))
                           (map-over-symbols proc (cdr exp)))) 
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))
(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter)) rule-counter)
(define (make-new-variable var rule-application-id) (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol (string-append "?"
                                 (if (number? (cadr variable))
                                     (string-append (symbol->string (caddr variable))
                                                    "-"
                                                    (number->string (cadr variable)))
                                     (symbol->string (cadr variable))))))

(define (execute exp)
  (proc-apply (eval (predicate exp) user-initial-environment)
              (args exp)))
;;frame representation
(define (make-binding variable value) (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define (display-stream s) (stream-for-each display-line s))
(define (display-line x) (newline) (display x))
;;table operations
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame-stream))))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed (qeval (first-disjunct disjuncts)
                                 frame-stream)
                          (delay (disjoin (rest-disjuncts disjuncts)
                                          frame-stream)))))

(define (negate operands frame-stream)
  (stream-flatmap (lambda (frame)
                    (if (stream-null? (qeval (negated-query operands)
                                             (singleton-stream frame)))
                        (singleton-stream frame)
                        the-empty-stream))
                  frame-stream))

(define (lisp-value call frame-stream)
  (stream-flatmap (lambda (frame)
                    (if (execute (instantiate call
                                              frame
                                              (lambda (v f)
                                                (error "Unknown pat var: LISP-VALUE" v))))
                        (singleton-stream frame)
                        the-empty-stream))
                  frame-stream))

(define (always-true ignore frame-stream) frame-stream)

(put 'not 'qeval negate)
(put 'and 'qeval conjoin)
(put 'or 'qeval disjoin)
(put 'lisp-value 'qeval lisp-value)
(put 'always-true 'qeval always-true)

;;driver loop
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp) (let ((binding (binding-in-frame exp frame)))
                        (if binding
                            (copy (binding-value binding))
                            (unbound-var-handler exp frame))))
      ((pair? exp) (cons (copy (car exp))
                         (copy (cdr exp))))
      (else exp))) (copy exp))

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q) (add-rule-or-assertion! (add-assertion-body q))
                                      (newline)
                                      (display "Assertion added to data base.")
                                      (query-driver-loop))
      (else (newline)
            (display output-prompt)
            (display-stream (stream-map (lambda (frame)
                                          (instantiate q
                                                       frame
                                                       (lambda (v f) (contract-question-mark v))))
                                        (qeval q (singleton-stream '()))))
            (query-driver-loop)))))
                                                       

;;underlying scheme evaluator
      (define *scheme-op-table* (make-hash-table))

      (define (scheme-put type proc)
        (hash-table/put! *scheme-op-table* type proc))

      (define (scheme-get type)
        (hash-table/get *scheme-op-table* type #f))

      (define (exp-type exp) (car exp))
      (define (eval exp env)
        (cond ((self-evaluating? exp) exp)
              ((variable? exp) (lookup-variable-value exp env))
              ((scheme-get (exp-type exp)) ((scheme-get (exp-type exp)) exp env))
              ((application? exp)
              (proc-apply (eval (operator exp) env)
                      (list-of-values (operands exp) env)))
              (else
                (error "Unknown expression type: EVAL" exp))))

;;specific definitions
      (define primitive-procedures 
        (list (list 'car car)
              (list 'cdr cdr)
              (list 'cons cons)
              (list 'null? null?)
              (list 'list list)
              (list '+ +)
              (list '- -)
              (list '* *)
              (list '/ /)
              (list '= =)))
      (define apply-in-underlying-scheme apply)

      (define (proc-apply procedure arguments)
        (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
              ((compound-procedure? procedure) (eval-sequence (procedure-body procedure)
                                                              (extend-environment (procedure-parameters procedure)
                                                                                  arguments
                                                                                  (procedure-environment procedure))))
              (else (error "Unknown procedure type: APPLY" procedure))))

      (define (tagged-list? exp tag) (if (pair? exp)
                  (eq? (car exp) tag)
                  false))

      (define (list-of-values exps env)
        (if (no-operands? exps)
            '()
            (cons (eval (first-operand exps) env)
                  (list-of-values (rest-operands exps) env))))


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
      (define (make-procedure parameters body env) (list 'procedure parameters body env))
      (define (compound-procedure? p) (tagged-list? p 'procedure))
      (define (procedure-parameters p) (cadr p))
      (define (procedure-body p) (caddr p))
      (define (procedure-environment p) (cadddr p))
      (define (eval-sequence exps env)
        (cond ((last-exp? exps) (eval (first-exp exps) env))
              (else (eval (first-exp exps) env) (eval-sequence (rest-exps exps) env))))

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
                    (cdr (frame-binding var frame))
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

      ;;table operations
      (define (assignment-variable exp) (cadr exp))
      (define (assignment-value exp) (caddr exp))
      (define (eval-assignment exp env)
        (set-variable-value! (assignment-variable exp)
                            (eval (assignment-value exp) env)
                            env)
        'ok)
      (define (assignment? exp) (tagged-list? exp 'set!))

      (define (definition-variable exp)
        (if (symbol? (cadr exp))
          (cadr exp)
          (caadr exp)))
      (define (definition-value exp)
        (if (symbol? (cadr exp)) (caddr exp)
          (make-lambda (cdadr exp)
                        (cddr exp))))
      (define (eval-definition exp env)
        (define-variable! (definition-variable exp)
                        (eval (definition-value exp) env)
                        env)
        'ok)
      (define (definition? exp) (tagged-list? exp 'define))

      (define (text-of-quotation exp) (cadr exp))
      (define (quoted? exp) (tagged-list? exp 'quote))

      (define (lambda-parameters exp) (cadr exp))
      (define (lambda-body exp) (cddr exp))
      (define (make-lambda parameters body) (cons 'lambda (cons parameters body)))
      (define (lambda? exp) (tagged-list? exp 'lambda))

      (define (if-predicate exp) (cadr exp))
      (define (if-consequent exp) (caddr exp))
      (define (if-alternative exp)
        (if (not (null? (cdddr exp)))
          (cadddr exp)
          'false))
      (define (make-if predicate consequent alternative)
        (list 'if predicate consequent alternative))
      (define (eval-if exp env)
        (if (true? (eval (if-predicate exp) env))
          (eval (if-consequent exp) env)
          (eval (if-alternative exp) env)))
      (define (if? exp) (tagged-list? exp 'if))


      (define (begin-actions exp) (cdr exp))
      (define (last-exp? seq) (null? (cdr seq)))
      (define (first-exp seq) (car seq))
      (define (rest-exps seq) (cdr seq))
      (define (sequence->exp seq)
        (cond ((null? seq) seq)
              ((last-exp? seq) (first-exp seq))
              (else (make-begin seq))))
      (define (make-begin seq) (cons 'begin seq))
      (define (begin? exp) (tagged-list? exp 'begin))

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
      (define (cond? exp) (tagged-list? exp 'cond))

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

      (define (and-exps exp) (cdr exp))
      (define (first-and exp) (car exp))
      (define (rest-and exp) (cdr exp))
      (define (and? exp) (tagged-list exp 'and))
      (define (eval-and exp env)
        (let ((expression (and-exps exp)))
          (if (null? expression)
            (error: "null and expression EVAL-AND"))
          (define (iter exp)
            (cond ((null? exp) #t)
                  ((not (true? (eval (first-and exp) env))) #f)
                  (else (iter (rest-and exp)))))
          (iter expression)))

      (define (or-exps exp) (cdr exp))
      (define (first-or exp) (car exp))
      (define (rest-or exp) (cdr exp))
      (define (or? exp) (tagged-list? exp 'or))
      (define (eval-or exp env)
        (let ((expression (or-exps exp)))
          (define (iter exp)
            (cond ((null? exp) #f)
                  ((true? (eval (first-or exp) env)) #t)
                  (else (iter (rest-or exp)))))
          (iter expression)))
        
      (define (let-assignments exp) (cadr exp))
      (define (let-body exp) (cddr exp))
      (define (let-vars exp) (map car (let-assignments exp)))
      (define (let-vals exp) (map cadr (let-assignments exp)))
      (define (make-let parameters body)
        (cons 'let (cons parameters body)))
      (define (let? exp) (tagged-list? exp 'let))
      (define (named-let? exp)
        (and (tagged-list? exp 'let))
        (symbol? (cadr exp)))
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

      (define (let*-parameters exp) (cadr exp))
      (define (let*-body exp) (cddr exp))
      (define (first-let* parameters) (car parameters))
      (define (rest-let* parameters) (car parameters))
      (define (let*? exp) (tagged-list? exp 'let*))
      (define (let*->nested-lets exp)
        (define (iter parameters body)
          (cond ((null? parameters) body) 
                (else (make-let (first-let* parameters)
                                (iter (rest-let* parameters))))))
        (iter (let*-parameters exp) (let*-body exp)))



        

      (scheme-put 'quote (lambda (exp env) (text-of-quotation exp)))
      (scheme-put 'set! eval-assignment)
      (scheme-put 'define eval-definition)
      (scheme-put 'if eval-if)
      (scheme-put 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
                                                      (lambda-body exp)
                                                      env)))
      (scheme-put 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
      (scheme-put 'cond (lambda (exp env) (eval (cond->if exp) env)))
      (scheme-put 'and eval-and)
      (scheme-put 'or eval-or)
      (scheme-put 'let (lambda (exp env) (eval (let->combination exp) env)))
      (scheme-put 'let* (lambda (exp env) (eval (let*->nested-lets exp) env)))


      ;global environment
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
      (define the-global-environment (setup-environment))

(query-driver-loop)
