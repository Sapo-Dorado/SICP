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
    (define (make-sparse variable term-list) (cons variable term-list))
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
    (define (make-dense variable term-list) (cons variable term-list))
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

  




  (define (variable poly)
    (apply-generic 'variable poly))
  (define (term-list poly)
    (apply-generic 'term-list poly))

  (define (order term type)
    ((get 'order type) term))
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
      (cond ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2))
                  (picked-type (pick-type type1 type2)))
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
                                (rest-terms L2) type1 type2) picked-type)))))))

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

  (define (negate-terms list-terms type1)
    (if (null? list-terms)
      '()
      (let ((coefficient (coeff (car list-terms) type1))
            (power (order list-terms type1)))
        (cons (make-term power (- coefficient) type1) (negate-terms (cdr list-terms) type1)))))

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
      (let ((negated ((make-type (pick-type (type poly1) (type poly2))) (variable poly1) (negate-terms (term-list poly2) (type poly2)))))
        (add-poly negated poly1))
       (error "Poly's not of same variable")))
  
  (define (tag p)
    (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'sub '(polynomial polynomial) (lambda (x y) (tag (sub-poly x y))))
  (put 'make 'sparse-polynomial (lambda (var terms) (tag (make-sparse var terms))))
  (put 'make 'dense-polynomial (lambda (var terms) (tag (make-dense var terms)))))

  
(install-polynomial-package)

(define (make-sparse-polynomial var terms)
  ((get 'make 'sparse-polynomial) var terms))

(define (make-dense-polynomial var terms)
  ((get 'make 'dense-polynomial) var terms))

(define (make-term order coeff)
  ((get 'make 'term) order coeff))
