(define (mangle-infinite x)
  (let ((list x))
    (define (set-custom! list sym num)
      (define (try list num)
        (if (= num 0)
          list
          (try (cdr list) (- num 1))))
      (set-car! (try list num) sym))
    (define (mangle x num)
      (if (and (pair? x) (not (pair? (car x))) (eq? (car x) '~))
        #t
        (if (pair? x)
          (begin (set-custom! list '~ num) (mangle (cdr x) (+ num 1)))
          #f)))
    (mangle list 0)))

(define a '(a b c))
(define b (cons a a))
(define c (cons b b))
(set-cdr! (last-pair c) c)

(mangle-infinite c)
