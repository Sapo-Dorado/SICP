(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) (lambda (value)
                                  (if trace
                                      (begin (display contents)
                                        (newline)))
                                  (set! contents value)))
            ((eq? message 'toggle-trace) (set! trace (not trace)))
            (else (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))


(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1)) top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0) 'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes 
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
        ((eq? message 'pop) (pop))
        ((eq? message 'initialize) (initialize))
        ((eq? message 'print-statistics)
         (print-statistics))
        (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-count 0)
        (trace #f))
    (define (instruction-inc)
      (set! instruction-count (+ instruction-count 1)))
    (define (print-instruction-count)
      (newline)
      (display (list 'instruction-count instruction-count))
      (set! instruction-count 0))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))
                  (list 'print-instruction-count
                        (lambda () (print-instruction-count)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
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
                (if (not (breakpoint? (car insts)))
                    (begin 
                      ((instruction-execution-proc (car insts)))
                      (instruction-inc)
                      (if trace
                          (begin (newline)
                            (display (caar insts))))
                      (execute))
                    (begin (advance-pc pc)
                      (newline)
                      (display "reached a breakpoint"))))))
      (define (breakpoint? exp)
        (tagged-list? exp 'breakpoint))
      (define (add-breakpoint position)
        (define (get-items list)
          (if (null? list)
              '()
              (cons (car list) (get-items (cdr list)))))
        (define (iter position commands)
          (if (= position 0)
              (begin (let ((end (get-items (cdr commands))))
                       (set-cdr! commands (cons (list 'breakpoint) end))))
              (if (breakpoint? (car commands))
                  (iter position (cdr commands))
                  (iter (- position 1) (cdr commands)))))
        (iter position the-instruction-sequence))
      (define (remove-breakpoint position)
        (define (iter position commands)
          (cond ((= position 0) (if (breakpoint? (car commands)) (set! the-instruction-sequence (cdr the-instruction-sequence))))
            ((= position 1) (if (breakpoint? (cadr commands)) (set-cdr! commands (cddr commands)) (cadr commands)))
            (else (if (breakpoint? (car commands)) (iter position (cdr commands)) (iter (- position 1) (cdr commands))))))
        (iter (+ position 1) the-instruction-sequence))
      (define (remove-all-breakpoints sequence)
        (cond ((or (null? sequence) (null? (cdr sequence))) 'done)
          ((breakpoint? (cadr sequence)) (set-cdr! sequence (cddr sequence)) (remove-all-breakpoints (cdr sequence)))
          (else (remove-all-breakpoints (cdr sequence)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
          ((eq? message 'continue) (execute))
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
          ((eq? message 'toggle-trace) (set! trace (not trace)))
          ((eq? message 'add-breakpoint) add-breakpoint)
          ((eq? message 'remove-breakpoint) remove-breakpoint)
          ((eq? message 'get-instruction-sequence) the-instruction-sequence)
          ((eq? message 'remove-all-breakpoints) (remove-all-breakpoints the-instruction-sequence))
          (else (error "Unknown request: MACHINE" message))))
      dispatch)))

(define (start machine) (machine 'start))
(define (continue machine) (machine 'continue))
(define (add-breakpoint machine pos) ((machine 'add-breakpoint) pos))
(define (remove-all-breakpoints machine) (machine 'remove-all-breakpoints))
(define (remove-breakpoint machine pos) ((machine 'remove-breakpoint) pos))
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
                 (make-primitive-exp e machine labels))
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

(define fact-machine
  (make-machine (list (list 'read read) (list '= =) (list '* *) (list '- -) (list 'print display) (list 'newline newline))
                '(controller
                   (perform (op newline))
                  (assign n (op read))
                  (assign continue (label fact-done))
                  fact-loop
                  (test (op =) (reg n) (const 1))
                  (branch (label base-case))
                  (save continue)
                  (save n)
                  (assign n (op -) (reg n) (const 1))
                  (assign continue (label after-fact))
                  (goto (label fact-loop))
                  after-fact
                  (restore n)
                  (restore continue)
                  (assign val (op *) (reg n) (reg val)) 
                  (goto (reg continue))
                  base-case
                  (assign val (const 1))
                  (goto (reg continue))
                  fact-done
                  (perform (op print) (reg val))
                  (perform (op print-stack-statistics))
                  (perform (op initialize-stack))
                  (perform (op print-instruction-count))
                  (goto (label controller)))))
 (add-breakpoint fact-machine 7)
(add-breakpoint fact-machine 5)
(add-breakpoint fact-machine 10)

(remove-breakpoint fact-machine 5)
(remove-breakpoint fact-machine 10)
(remove-breakpoint fact-machine 7)

(start fact-machine)
