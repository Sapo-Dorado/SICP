(define (ripple-carry-adder a-list b-list s-list c-out)
  (let ((inital-c (make-wire)))
    (set-signal! initial-c 0)
    (define (iter as bs ss c-in)
      (if (null? as)
        'ok
        (begin
          (full-adder (car as) (car bs) c-in (car ss) c-out)
          (iter (cdr as) (cdr bs) (cdr ss) c-out))))
  (iter a-list b-list s-list inital-c)))
