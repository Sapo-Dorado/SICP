(define f
  (let ((var 0))
    (lambda (num)
      (set! var num))))

(+ (f 0) (f 1))
