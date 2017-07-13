(define (last-pair x)
  (if (null? (cdr x))
      (car x)
      (last-pair (cdr x))))

(define (reversal x)
  (define (iter original new)
    (if (null? original)
               new
               (iter (cdr original) (cons (car original) new))))
  (iter x ()))


(define (same-parity-iter . x)
  (define (find-parity y)
    (remainder y 2))
  (define parity (find-parity (car x)))
  (define (test x)
    (= (find-parity x) parity))
  (define (iter x ans)
          (cond ((null? x) ans)
                ((test (car x)) (iter (cdr x) (append (list (car x) ans))))
                (else (iter (cdr x) ans))))
        (iter x ()))

(define (same-parity . x)
  (define (find-parity y)
    (remainder y 2))
  (define parity (find-parity (car x)))
  (define (test x)
    (= (find-parity x) parity))
  (define (not-iter x)
    (cond ((null? x) ())
          ((test (car x)) (cons (car x) (not-iter (cdr x))))
          (else (not-iter (cdr x)))))
  (not-iter x))


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items '()))


(define (for-each proc items)
  (define (iter items)
    (cond ((null? items) nil)
          (else (proc (car items)) (iter (cdr items)))))
  (iter items))

(define nil ())

(define (foreach proc lst)
  (define (iter items)
    (cond ((null? items) nil)
          (else (proc (car items)) (iter (cdr items)))))
  (iter lst))

(reversal (list 1 2 3 4 5 6 7))
