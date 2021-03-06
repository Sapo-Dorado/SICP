(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

(define (attach-tag type-tag contents) 
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    'scheme-number)) 

(define (contents datum)
  (if (pair? datum) (cdr datum)
    datum))










(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
      (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number)
    (lambda (x) (= x 0)))
  (put 'exp '(scheme-number scheme-number)
    (lambda (x y) (tag (expt x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
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
      (+ (real-part x) (real-part y) (real-part z))))
  (put 'make-from-real-imag 'complex
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
    (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)










(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define put-coercion put)
(define get-coercion get)

(define (scheme-number->complex n) (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

(define (apply-generic op . args)

  (define (coerce args tried-args)
    (define (try type args ans)
      (if (null? args)
        ans
        (if (eq? type (type-tag (car args)))
          (try type (cdr args) (append ans (list (car args))))
          (let ((got (get-coercion (type-tag (car args)) type)))
            (if got
              (try type (cdr args) (append ans (list (got (car args)))))
            #f)))))
    (if (null? args)
      #f
      (let ((attempt (try (type-tag (car args)) (append tried-args args) '())))
        (if attempt
          attempt
          (coerce (cdr args) (append tried-args (list (car args))))))))

    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (let ((coerce-attempt (coerce args '())))
            (if coerce-attempt
              (let ((new-type-tags (map type-tag coerce-attempt)))
                (let ((new-proc (get op new-type-tags)))
                  (if new-proc
                    (apply new-proc (map contents coerce-attempt))
                    (error "coerce unsuccessful"))))
              (error "Failed to coerce")))))))
        
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (complex-specific-op x y z) (apply-generic 'complex-specific-op x y z))
(define (exp x y) (apply-generic 'exp x y))


(define a (make-rational 10 20))
(define b (make-scheme-number 20))
(define c (make-scheme-number 20))


  (define (coerce args tried-args)
    (define (try type args ans)
      (if (null? args)
        ans
        (if (eq? type (type-tag (car args)))
          (try type (cdr args) (append ans (list (car args))))
          (let ((got (get-coercion (type-tag (car args)) type)))
            (if got
              (try type (cdr args) (append ans (list (got (car args)))))
            #f)))))
    (if (null? args)
      #f
      (let ((attempt (try (type-tag (car args)) (append tried-args args) '())))
        (if attempt
          attempt
          (coerce (cdr args) (append tried-args (list (car args))))))))

(complex-specific-op c a b)
