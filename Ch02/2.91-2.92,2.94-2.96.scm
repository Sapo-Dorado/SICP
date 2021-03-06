(load "arithmetic-library.scm")

(define (install-polynomial-package)
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (type poly) (car poly))
  (define (same-type? p1 p2)
    (eq? (type p1) (type p2)))
  (define (pick-type type1 type2)
    (if (same-variable? type1 type2)
      type1
      'sparse))

  
  (define (install-sparse-package)
    (define (make-sparse variable term-list)
      (if (and (not (null? term-list)) (=zero? (coeff (first-term term-list))))
        (make-sparse variable (rest-terms term-list))
        (cons variable term-list)))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (order terms) (car (car terms)))
    (define (coeff term) (cadr term))
    (define (make-term order coeff) (list order coeff))
    (define (adjoin-term term term-list)
      (if (=zero? (coeff term))
          term-list
          (cons term term-list)))
    (define (tag x) (attach-tag 'sparse x))
    (put 'variable '(sparse) variable)
    (put 'term-list '(sparse) term-list)
    (put 'order 'sparse order)
    (put 'coeff 'sparse coeff)
    (put 'make-term 'sparse make-term)
    (put 'adjoin-term 'sparse adjoin-term)
    (put 'make 'sparse
        (lambda (var terms) (tag (make-sparse var terms))))
    (put 'make-term 'sparse make-term))
  (install-sparse-package)
  
  (define (install-dense-package)
    (define (make-dense variable term-list) 
      (if (and (not (null? term-list)) (=zero? (coeff (first-term term-list))))
        (make-dense variable (rest-terms term-list))
        (cons variable term-list)))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (order terms) (- (length terms) 1))
    (define (coeff term) term)
    (define (make-term order coeff) coeff)
    (define (adjoin-term term term-list)
      (cons term term-list))
    (define (tag x) (attach-tag 'dense x))
    (put 'variable '(dense) variable)
    (put 'term-list '(dense) term-list)
    (put 'order 'dense order)
    (put 'coeff 'dense coeff)
    (put 'make-term 'dense make-term)
    (put 'adjoin-term 'dense adjoin-term)
    (put 'make 'dense 
         (lambda (var terms) (tag (make-dense var terms))))
    (put 'make-term 'dense make-term))
  (install-dense-package)


  (define (install-coersion-package)
    (define (dense-terms->sparse-terms term-list)
        (if (null? term-list)
         '() 
          (let ((new-o (order term-list 'dense))
                (new-c (coeff (first-term term-list) 'dense)))
            (let ((term (make-term new-o new-c 'sparse)))
              (adjoin-term term (dense-terms->sparse-terms (rest-terms term-list)) 'sparse)))))
    (put 'coerce->sparse 'dense dense-terms->sparse-terms))
  (install-coersion-package)

  (define (coerce->sparse term-list type)
    ((get 'coerce->sparse type) term-list))

  (define (variable poly)
    (apply-generic 'variable poly))
  (define (term-list poly)
    (apply-generic 'term-list poly))

  (define (order terms type)
    ((get 'order type) terms))
  (define (coeff term type)
    ((get 'coeff type) term))
  (define (make-term order coeff type)
    ((get 'make-term type) order coeff))

  (define (make-sparse var terms)
    ((get 'make 'sparse) var terms))
  (define (make-dense var terms)
    ((get 'make 'dense) var terms))

  (define (make-type type)
    (get 'make type))

  (define (add-terms L1 L2 type1 type2)
    (let ((picked-type (pick-type type1 type2)))
      (cond ((empty-termlist? L1) (if (eq? type2 picked-type)L2 (coerce->sparse L2 type2)))
            ((empty-termlist? L2) (if (eq? type1 picked-type) L1 (coerce->sparse L1 type1)))
            (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order L1 type1) (order L2 type2))
                      (adjoin-term
                      (make-term (order L1 type1) (coeff t1 type1) picked-type) (add-terms (rest-terms L1) L2 type1 type2) picked-type))
                    ((< (order L1 type1) (order L2 type2))
                      (adjoin-term
                      (make-term (order L2 type2) (coeff t2 type2) picked-type) (add-terms L1 (rest-terms L2) type1 type2) picked-type))
                    (else
                      (adjoin-term
                      (make-term  (order L1 type1)
                                  (add (coeff t1 type1) (coeff t2 type2))
                                  picked-type)
                      (add-terms (rest-terms L1)
                                (rest-terms L2) type1 type2) picked-type))))))))

  (define (mul-terms L1 L2 type1 type2)
   (if (empty-termlist? L1)
      (the-empty-termlist)
      (let ((picked-type 'sparse))
        (add-terms (mul-term-by-all-terms L1 L2 type1 type2)
                  (mul-terms (rest-terms L1) L2 type1 type2) picked-type picked-type))))

  (define (mul-term-by-all-terms orderList L type1 type2)
    (if (empty-termlist? L)
          (the-empty-termlist)
          (let ((t2 (first-term L))
                (t1 (first-term orderList))
                (picked-type 'sparse))
            (adjoin-term
            (make-term (+ (order orderList type1) (order L type2))
                        (mul (coeff t1 type1) (coeff t2 type2)) picked-type)
           (mul-term-by-all-terms orderList (rest-terms L) type1 type2) picked-type))))
      
  (define (adjoin-term term term-list type)
    ((get 'adjoin-term type) term term-list))

  (define (negate-terms list-terms type1 desired-type)
    (if (null? list-terms)
      '()
      (let ((coefficient (coeff (car list-terms) type1))
            (power (order list-terms type1)))
        (cons (make-term power (- coefficient) desired-type) (negate-terms (cdr list-terms) type1 desired-type)))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      ((make-type (pick-type (type p1) (type p2))) (variable p1) 
                (add-terms (term-list p1) (term-list p2) (type p1) (type p2)))
      (error "Poly's not in same var: ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      ((make-type 'sparse) (variable p1) 
                 (mul-terms (term-list p1) (term-list p2) (type p1) (type p2)))
      (error "Poly's not in same var: ADD-POLY" (list p1 p2))))

  (define (=zero?-poly poly)
    (define (map-coeff type)
      (get 'coeff type))
    (let ((terms (term-list poly)))
      (let ((coeffs (map (map-coeff (type poly)) terms)))
        (let ((abscoeffs (map abs coeffs)))
          (let ((sum (apply + abscoeffs)))
            (if (= sum 0)
              #t
              #f))))))

  (define (sub-poly poly1 poly2)
    (if (same-variable? (variable poly1) (variable poly2))
      (let ((picked-type (pick-type (type poly1) (type poly2))))
        (let ((negated ((make-type picked-type) (variable poly1) (negate-terms (term-list poly2) (type poly2) picked-type))))
        (add-poly negated poly1)))
       (error "Poly's not of same variable")))

  (define (div-terms L1 L2 type1 type2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order L2 type2) (order L1 type1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1 type1) (coeff t2 type2)))
                    (new-o (- (order L1 type1) (order L2 type2))))
                (let ((rest-of-result
                      (div-terms 
                        (add-terms L1 
                                   (negate-terms
                                     (mul-terms (list (make-term new-o new-c 'sparse)) L2 'sparse type2)
                                                'sparse 'sparse)
                                     type1 'sparse)
                        L2
                        'sparse type2)))
                  (list (cons (make-term new-o new-c 'sparse) (car rest-of-result)) (cadr rest-of-result))))))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2)) 
                   (div-terms (term-list p1) (term-list p2) (type p1) (type p2))))

  (define (equ?-poly p1 p2)
    (define (iter terms1 terms2 type1 type2)
      (if (null? terms1)
        (if (=zero? (attach-tag 'polynomial ((make-type type2) (variable p2) terms2))) #t #f)
        (if (null? terms2)
          (if (=zero? (attach-tag 'polynomial ((make-type type1) (variable p1) terms1))) #t #f)
          (let ((c1 (coeff (first-term terms1) type1))
                (c2 (coeff (first-term terms2) type2))
                (o1 (order terms1 type1))
                (o2 (order terms2 type2)))
                  (if (and (equ? c1 c2) (equ? o1 o2))
                    (iter (rest-terms terms1) (rest-terms terms2) type1 type2)
                    (if (> o1 o2)
                      (if (=zero? c1)
                        (iter (rest-terms terms1) terms2 type1 type2)
                        #f)
                        (if (> o2 o1)
                          (if (=zero? c2)
                            (iter terms1 (rest-terms terms2) type1 type2)
                            #f)
                          #f)))))))
    (if (same-variable? (variable p1) (variable p2))
      (iter (term-list p1) (term-list p2) (type p1) (type p2))
      #f))

  (define (gcd-terms a b type1 type2)
    (define (div-by-constant k terms)
      (if (null? terms)
        terms
        (let ((o (order terms type1))
              (c (coeff (first-term terms) type1)))
          (adjoin-term (make-term o (div c k) type1) (div-by-constant k (rest-terms terms)) type1))))
    (define (reduce x)
      (define (mcoeff x)
        (coeff x type1))
      (let ((coeffs (map mcoeff a)))
        (define (iter term rest ans)
          (if (null? rest)
            ans
            (iter (first-term rest) (rest-terms rest) (gcd term ans))))
        (if (null? coeffs)
          coeffs
          (let ((term (iter (first-term coeffs) (rest-terms coeffs) (first-term coeffs))))
            (div-by-constant term x)))))
    (if (empty-termlist? b)
      (reduce a)
      (gcd-terms b (pseudoremainder-terms a b type1 type2) type1 type2)))

  (define (pseudoremainder-terms a b type1 type2)
    (define (mul-by-constant k terms)
      (if (null? terms)
        terms
        (let ((o (order terms type1))
              (c (coeff (first-term terms) type1)))
          (adjoin-term (make-term o (mul k c) type1) (mul-by-constant k (rest-terms terms)) type1))))
    (define (integerizing-factor x y)
      (let ((o1 (order x type1))
            (o2 (order y type2))
            (c (coeff (first-term y) type2)))
            (expt c (- (+ 1 o1) o2))))
    (let ((i (integerizing-factor a b)))
      (cadr (div-terms (mul-by-constant i a) b type1 type2))))

  (define (gcd-poly p1 p2)
    (let ((type1 (type p1))
          (type2 (type p2)))
      (if (same-variable? (variable p1) (variable p2))
        (if (and (eq? (type p1) 'sparse) (eq? (type p2) 'sparse))
          ((make-type 'sparse) (variable p1) (gcd-terms (term-list p1) (term-list p2) 'sparse 'sparse))
          (if (and (not (eq? (type p1) 'sparse)) (eq? (type p2) 'sparse))
            ((make-type 'sparse) (variable p1) (gcd-terms (coerce->sparse (term-list p1) type1) (term-list p2) 'sparse 'sparse))
            (if (and (not (eq? (type p2) 'sparse)) (eq? (type p1) 'sparse))
              ((make-type 'sparse) (variable p1) (gcd-terms (term-list p1) (coerce->sparse (term-list p2) type2) 'sparse 'sparse))
              (if (and (not (eq? (type p2) 'sparse)) (not (eq? (type p1) 'sparse)))
                ((make-type 'sparse) (variable p1) (gcd-terms (coerce->sparse (term-list p1) type1) (coerce->sparse (term-list p2) type2) 'sparse 'sparse)))))))))


  (define (tag p)
    (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'sub '(polynomial polynomial) (lambda (x y) (tag (sub-poly x y))))
  (put 'make 'sparse-polynomial (lambda (var terms) (tag (make-sparse var terms))))
  (put 'make 'dense-polynomial (lambda (var terms) (tag (make-dense var terms))))
  (put 'div '(polynomial polynomial) (lambda (x y) (div-poly x y)))
  (put 'equ? '(polynomial polynomial) equ?-poly)
  (put 'gcd '(polynomial polynomial) gcd-poly))
  
(install-polynomial-package)

(define (make-sparse-polynomial var terms)
  ((get 'make 'sparse-polynomial) var terms))

(define (make-dense-polynomial var terms)
  ((get 'make 'dense-polynomial) var terms))

(define (make-term order coeff)
  ((get 'make 'term) order coeff))

(define p1 (make-sparse-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-sparse-polynomial 'x '((2 11) (0 7))))
(define p3 (make-sparse-polynomial 'x '((1 13) (0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(gcd q1 q2)
