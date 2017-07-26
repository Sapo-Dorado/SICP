(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) '()))

(define (=number? x y)
  (and (and (number? x) (number? y)) (= x y)))

(define (same-variable? exp var)
  (eq? exp var))

(define (variable? x)
  (and (not (pair? x)) (not (number? x))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (list (operator exp))) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


(define (install-derivative-package)
  (define (make-sum s1 s2)
    (cond ((=number? s1 0) s2)
          ((=number? s2 0) s1)
          ((and (number? s1) (number? s2)) (+ s1 s2))
          (else (list '+ s1 s2))))

  (define (make-mult p1 p2)
    (cond ((or (=number? p1 0) (=number? p2 0)) 0)
          ((=number? p1 1) p2)
          ((=number? p2 1) p1)
          ((and (number? p1) (number? p2)) (* p1 p2))
          (else (list '* p1 p2))))

  (define (make-exp base power)
    (cond ((=number? power 0) 1)
          ((=number? power 1) base)
          ((and (number? base) (number? power)) (expt base power))
          (else (list '** base power))))

  (define (addend exp)
    (car exp))

  (define (augend exp)
    (cadr exp))

  (define (multiplier exp)
    (car exp))

  (define (multiplicand exp)
    (cadr exp))

  (define (base exp)
    (car exp))

  (define (power exp)
    (cadr exp))
    
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (define (deriv-mult exp var)
    (make-sum (make-mult (deriv (multiplier exp) var)
                         (multiplicand exp))
              (make-mult (multiplier exp)
                         (deriv (multiplicand exp) var))))

  (define (deriv-exp exp var)
    (make-mult
      (make-mult (power exp)
                 (make-exp (base exp) (- (power exp) 1)))
      (deriv (base exp) var)))

  (put 'deriv '(+) deriv-sum)
  (put 'deriv '(*) deriv-mult)
  (put 'deriv '(**) deriv-exp))

(install-derivative-package)

(deriv '(* x (+ x x)) 'x)


