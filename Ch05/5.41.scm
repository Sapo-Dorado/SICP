(define (find-variable var ct-env)
  (define (find-var var frame)
    (define (scan pos frame)
      (cond ((null? frame) #f)
            ((eq? (car frame) var) pos)
            (else (scan (+ pos 1) (cdr frame)))))
    (scan 0 frame))
  (define (scan pos env)
    (if (null? env)
        'not-found
        (let ((look (find-var var (car env))))
          (if look
              (cons pos look)
              (scan (+ pos 1) (cdr env))))))
  (scan 0 ct-env))


(find-variable 'c '((y z) (a b c d e) (x y)))
(find-variable 'x '((y z) (a b c d e) (x y)))
(find-variable 'w '((y z) (a b c d e) (x y)))
