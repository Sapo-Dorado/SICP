(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

(define (attach-tag type-tag contents) 
  (if (eq? type-tag 'integer)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    'integer)) 

(define (contents datum)
  (if (pair? datum) (cdr datum)
    datum))










(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer)
      (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
        (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
        (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
        (lambda (x y) (tag (/ x y))))
  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer)
    (lambda (x) (= x 0)))
  (put 'exp '(integer integer)
    (lambda (x y) (tag (expt x y))))
  (put 'make 'integer (lambda (x) (tag (round x))))
  'done)





(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
      (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
        (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
        (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
        (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real) =)
  (put '=zero? '(real)
    (lambda (x) (= x 0)))
  (put 'exp '(real real)
    (lambda (x y) (tag (expt x y))))
  (put 'make 'real (lambda (x) (tag x)))
  'done)



















(define (install-rational-package)
  (define (numer x) (car x)) (define (denom x) (cdr x)) (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                  (* (numer y) (denom x)))
                  (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
                 (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
      (make-rat (* (numer x) (denom y))
                (* (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
    (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
    (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
    (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
    (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
    (lambda (x y) (= (/ (numer x) (denom x)) (/ (numer y) (denom y)))))
  (put '=zero? '(rational)
    (lambda (x) (= (numer x) 0)))
  (put 'make 'rational
    (lambda (n d) (tag (make-rat n d))))
  'done)













(define (install-complex-package)
  (define (install-polar-package)
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))

    (define (real-part z) (* (magnitude z) (cos (angle z))))
    (define (imag-part z) (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y)))
              (atan y x)))

    (define (tag x) (attach-tag 'polar x))

    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
      (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
      (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)
  (install-polar-package)

  (define (install-rectangular-package)

    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y)) (define (magnitude z)
        (sqrt (+ (square (real-part z))
                (square (imag-part z)))))
    (define (angle z) (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)

    (put 'make-from-real-imag 'rectangular
    (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
    (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)

  (install-rectangular-package)
    
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))


  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
      (make-from-real-imag (+ (real-part z1) (real-part z2))
                          (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
      (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                        (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                        (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z)) 258
  (put 'add '(complex complex)
    (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
    (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
    (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
    (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
    (lambda (x y) (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
    (lambda (x) (and (= (real-part x) 0) (= (imag-part x) 0))))
  (put 'complex-specific-op '(complex complex complex)
    (lambda (x y z)
      (make-complex-from-real-imag (+ (real-part x) (real-part y) (real-part z)) 0)))
  (put 'make-from-real-imag 'complex
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
    (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  'done)






(define (install-raise-package)
  (define (raise-integer x)
      (make-rational x 1))

  (define (raise-rational x)
    (make-real (/ (car x) (cdr x))))

  (define (raise-real x)
      (make-complex-from-real-imag x 0))

  (put 'raise '(integer) raise-integer)
  (put 'raise '(rational) raise-rational)
  (put 'raise '(real) raise-real)
  (put 'value 'integer 1)
  (put 'value 'rational 2)
  (put 'value 'real 3)
  (put 'value 'complex 4))





(define (install-project-package)
  (define (project-rational x)
    (let ((projection (make-integer (/ (car x) (cdr x)))))
       (if (equ? projection (attach-tag 'rational x))
         projection
         #f)))
  (define (project-real x)
    (let ((projection (make-rational (round x) 1)))
      (if (equ? projection (attach-tag 'real x))
        projection
        #f)))
  (define (project-complex x)
    (let ((projection (make-real (real-part x))))
      (if (equ? projection (attach-tag 'complex x))
        projection
        #f)))
  (put 'project '(rational) project-rational)
  (put 'project '(real) project-real)
  (put 'project '(complex) project-complex))


(define (drop x)
  (if (eq? (type-tag x) 'integer)
    x
    (let ((projection (project x)))
      (if projection
        (drop projection)
        x))))





(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-raise-package)
(install-project-package)

(define put-coercion put)
(define get-coercion get)

(define (integer->complex n) (make-complex-from-real-imag (contents n) 0))
(put-coercion 'integer
              'complex
              integer->complex)

(define (apply-generic op . args)

    (define (find-value x)
      (get 'value (type-tag x)))
    (define (find-highest-value args value)
      (cond ((null? args) value)
            ((> (find-value (car args)) value) (find-highest-value (cdr args) (find-value (car args))))
            (else (find-highest-value (cdr args) value))))

  (define (coerce-raise args)
    (define (iter num args raised-args)
      (cond ((null? args) raised-args)
            ((= (find-value (car args)) num) (iter num (cdr args) (append raised-args (list (car args)))))
            (else (iter num (cons (raise (car args)) (cdr args)) raised-args))))
    (iter (find-highest-value args 0) args '()))

      (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
          (if proc
            (apply proc (map contents args))
            (let ((coerce-attempt (coerce-raise args)))
              (if coerce-attempt
                (let ((new-proc (get op (map type-tag coerce-attempt))))
                  (if new-proc
                    (drop (apply new-proc (map contents coerce-attempt)))
                    (error "Coercion unsuccessful")))
                (error "Coercion failed")))))))
        
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-integer n)
  ((get 'make 'integer) n))

(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))

(define (make-real x) ((get 'make 'real) x))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (complex-specific-op x y z) (apply-generic 'complex-specific-op x y z))
(define (exp x y) (apply-generic 'exp x y))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (real-part x) (apply-generic 'real-part x))


(define a (make-complex-from-real-imag 20 1))
(define b (make-rational 20 1))
(define c (make-integer 20))

