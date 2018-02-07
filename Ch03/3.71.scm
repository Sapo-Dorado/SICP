(load "stream-library.scm")
(define (cube x)
  (* x x x))
(define (ramanujan-weight x)
  (+ (cube (car x)) (cube (cadr x))))

(define cube-sum-list (weighted-pairs integers integers ramanujan-weight))

(define (ramanujan-numbers)
  (define (iter stream p-weight)
    (let ((weight (ramanujan-weight (stream-car stream))))
      (if (= weight p-weight)
          (cons-stream
            weight
            (iter (stream-cdr stream) weight))
        (iter (stream-cdr stream) weight))))
  (iter cube-sum-list 0))


(show-stream (ramanujan-numbers) 6)
