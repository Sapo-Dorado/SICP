(define (smallest-divisor n) 
  (find-divisor n 2))

(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (divides? a b)
  (= (remainder b a) 0))

(define nil ())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s) ; empty set?
      (list nil) ; sequence containing empty set 
      (flatmap (lambda (x)
        (map (lambda (p) (cons x p)) (permutations (remove x s))))
      s)))

(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (y)
               (list x y)) (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
                 (filter prime-sum?
                 (unique-pairs n))))


(define (triple n s)
  (define (good? seq)
    (= (+ (car seq) (cadr seq) (caddr seq)) s))
  (filter good?
          (flatmap (lambda (i)
                           (flatmap (lambda (j)
                                      (map (lambda (k)
                                             (list i j k))
                                           (enumerate-interval 1 (- j 1))))
                                    (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(triple 10 10)
