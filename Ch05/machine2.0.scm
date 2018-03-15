;;sources to a register can include duplicates I couldn't figure out a way to check similarity between the sources
;;edit: discovered the function member that works for what I need. I probably could have used it elsewhere to be cleaner
;;but i dont feel like making changes
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    ((machine 'set-instruction-list) (create-instruction-list controller-text))
    ((machine 'set-save-list) (create-save-list controller-text))
    ((machine 'set-entry-register-list) (create-entry-point-list controller-text))
    machine))
(define (create-instruction-list text)
  (let ((assigns '())
        (branches '())
        (gotos '())
        (saves '())
        (restores '()))
    (define (iter text)
      (cond ((null? text) (append assigns branches gotos saves restores))
            ((tagged-list? (car text) 'assign) (set! assigns (cons (car text) assigns))
                                              (iter (cdr text)))
            ((tagged-list? (car text) 'branch) (set! branches (cons (car text) branches))
                                              (iter (cdr text)))
            ((tagged-list? (car text) 'goto) (set! gotos (cons (car text) gotos))
                                              (iter (cdr text)))
            ((tagged-list? (car text) 'save) (set! saves (cons (car text) saves))
                                              (iter (cdr text)))
            ((tagged-list? (car text) 'restore) (set! restores (cons (car text) restores))
                                              (iter (cdr text)))
            (else (iter (cdr text)))))
    (iter text)))

(define (included? item list)
  (cond ((null? list) #f)
        ((eq? item (car list)) #t)
        (else (included? item (cdr list)))))

(define (create-save-list text)
  (define (iter saved-vars text)
    (cond ((null? text) saved-vars)
          ((and (tagged-list? (car text) 'save) (not (included? (cadar text) saved-vars))) (iter (cons (cadar text) saved-vars) (cdr text)))
          (else (iter saved-vars (cdr text)))))
  (iter '() text))

(define (create-entry-point-list text)
  (define (iter entry-points text)
    (cond ((null? text) entry-points)
          ((and (tagged-list? (car text) 'goto) (tagged-list? (cadar text) 'reg) (not (included? (car (cdadar text)) entry-points)))
            (iter (cons (car (cdadar text)) entry-points) (cdr text)))
          (else (iter entry-points (cdr text)))))
  (iter '() text))

(define (make-register name)
  (let ((contents '*unassigned*)
        (sources '()))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) (lambda (value) (set! contents value)))
            ((eq? message 'add-sources) (lambda (source)
                                          (if (not (member source sources))
                                              (set! sources (cons source sources)))))
            ((eq? message 'get-sources) sources)
            (else (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '()))
    (define (push x) (set! s (cons x s)))
    (define (pop) (if (null? s)
                      (error "Empty stack: POP")
                      (let ((top (car s)))
                        (set! s (cdr s))
                        top)))
    (define (initialize) (set! s '()) 'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
        ((eq? message 'pop) (pop))
        ((eq? message 'initialize) (initialize))
        (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
              (stack (make-stack))
              (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag)))
          (instruction-list '())
          (entry-register-list '())
          (save-list '()))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table (cons (list name (make-register name))
                                       register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (begin (allocate-register name)
                (lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin ((instruction-execution-proc (car insts))) (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
          ((eq? message 'install-instruction-sequence)
           (lambda (seq)
             (set! the-instruction-sequence seq)))
          ((eq? message 'allocate-register)
           allocate-register)
          ((eq? message 'get-register)
           lookup-register)
          ((eq? message 'install-operations) (lambda (ops)
                                               (set! the-ops (append the-ops ops)))) ((eq? message 'stack) stack)
          ((eq? message 'operations) the-ops)
          ((eq? message 'set-instruction-list) (lambda (vals) (set! instruction-list vals)))
          ((eq? message 'get-instruction-list) instruction-list)
          ((eq? message 'set-entry-register-list) (lambda (vals) (set! entry-register-list vals)))
          ((eq? message 'get-entry-register-list) entry-register-list)
          ((eq? message 'set-save-list) (lambda (vals) (set! save-list vals)))
          ((eq? message 'get-save-list) save-list)
          (else (error "Unknown request: MACHINE" message))))
      dispatch)))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
        (cdr text)
        (lambda (insts labels)
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (receive insts
                         (cons (make-label-entry next-inst
                                                 insts)
                               labels))
                (receive (cons (make-instruction next-inst)
                               insts)
                         labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each (lambda (inst)
                (set-instruction-execution-proc!
                  inst
                  (make-execution-procedure
                    (instruction-text inst)
                    labels machine pc flag stack ops)))
              insts)))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts) (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"
               label-name))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else
          (error "Unknown instruction type: ASSEMBLE" inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    ((target 'add-sources) value-exp)
    (let ((value-proc
            (if (operation-exp? value-exp)
                (make-operation-exp
                  value-exp machine labels operations)
                (make-primitive-exp
                  (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc)) (advance-pc pc)))))

(define (assign-reg-name assign-instruction) (cadr assign-instruction))
(define (assign-value-exp assign-instruction) (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc)))) 

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
                (make-operation-exp
                  condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc)) (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))
        
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
                (lookup-label
                  labels
                  (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))

(define (branch-dest branch-instruction) (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels
                                      (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
      ((register-exp? dest) (let ((reg (get-register
                                         machine
                                         (register-exp-reg dest))))
                              (lambda ()
                                (set-contents! pc (get-contents reg)))))
      (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))

(define (goto-dest goto-instruction) (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg)) (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack)) (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction) (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
                (make-operation-exp
                  action machine labels operations)))
          (lambda () (action-proc) (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp))) (lambda () c)))
    ((label-exp? exp)
     (let ((insts (lookup-label
                    labels
                    (label-exp-label exp)))) (lambda () insts)))
    ((register-exp? exp)
     (let ((r (get-register machine (register-exp-reg exp))))
       (lambda () (get-contents r))))
    (else (error "Unknown expression type: ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
                         operations))
        (aprocs
          (map (lambda (e)
                 (if (label-exp? e)
                     (error "op applied onto a label make-operation:" e)
                     (make-primitive-exp e machine labels)))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp) (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp) (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

(define (tagged-list? exp tag) (if (pair? exp)
            (eq? (car exp) tag)
            false))

(define gcd-machine (make-machine
   (list (list 'rem remainder) (list '= =))
   '(test-b (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (op rem) (reg a) (reg b))
            (assign a (reg b))
            (assign b (reg t))
            (goto (label test-b))
            gcd-done)))
(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(start gcd-machine)
(get-register-contents gcd-machine 'a)
