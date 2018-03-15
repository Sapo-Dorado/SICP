(load "expmachine1.0.scm")
(define exp-machine
  (make-machine
    '(b n continue val)
    (list (list '= =) (list '+ +) (list '- -) (list '* *))
    '((assign continue (label exp-done))
      exp-loop
      (test (op =) (reg n) (const 0))
      (branch (label base-case))
      (save continue)
      (assign continue (label mul-loop))
      (assign n (op -) (reg n) (const 1))
      (goto (label exp-loop))
      mul-loop
      (assign val (op *) (reg val) (reg b))
      (restore continue)
      (goto (reg continue))
      base-case
      (assign val (const 1))
      (goto (reg continue))
      exp-done)))
(set-register-contents! exp-machine 'b 5)
(set-register-contents! exp-machine 'n 3)
(start exp-machine)
(get-register-contents exp-machine 'val)
