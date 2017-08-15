(define (check-infinite x)
  (define (checker list lists)
      (if (not (pair? list))
        #f
        (if (memq list lists)
          #t
          (begin (set! lists (cons list lists))
          (or (checker (car list) lists) (checker (cdr list) lists))))))
    (checker x '()))

(define a (list 'a 'b 'c))
(set-cdr! (last-pair a) a)
(define b (cons 'a a))

(check-infinite b)
