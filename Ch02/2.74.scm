(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) '()))

(define (find-tag record)
  (car record))
(define (contents record)
  (cdr record))

(define (install-record-package-a)
  (define (tag x)
    (cons 'a x))
  (define (add-employee name address salary record)
    (tag (cons (list name address salary) record)))
  (define (name item)
    (car item))
  (define (address item)
    (cadr item))
  (define (salary item)
    (caddr item))

  (define (find-employee employee record)
    (cond ((null? record) #f)
          ((eq? (name (car record)) employee) (car record))
          (else (find-employee employee (cdr record)))))

  (define (get-salary employee record)
    (salary (find-employee employee record)))

  (put 'a 'find-employee find-employee)
  (put 'a 'add-employee add-employee)
  (put 'a 'get-salary get-salary))

(define (install-record-package-b)
  (define (tag x)
    (cons 'b x))
  (define (add-employee name address salary record)
    (tag (cons (list salary name address) record)))
  (define (salary item)
    (car item))
  (define (name item)
    (cadr item))
  (define (address item)
    (caddr item))

  (define (find-employee employee record)
    (cond ((null? record) #f)
          ((eq? (name (car record)) employee) (car record))
          (else (find-employee employee (cdr record)))))

  (define (get-salary employee record)
    (salary (find-employee employee record)))

  (put 'b 'find-employee find-employee)
  (put 'b 'add-employee add-employee)
  (put 'b 'get-salary get-salary))

(define (find-employee employee record)
  ((get (find-tag record) 'find-employee) employee (contents record)))

(define (add-employee name address salary record)
  ((get (find-tag record) 'add-employee) name address salary (contents record)))

(define (get-salary employee record)
  ((get (find-tag record) 'get-salary) employee (contents record)))

(define (make-record version)
  (list version))

(define (find-employee-record employee all-files)
  (if (null? all-files)
    (error "Employee not found")
    (let ((check ((get (find-tag (car all-files)) 'find-employee) employee (contents (car all-files)))))
      (if check
        check
        (find-employee-record employee (cdr all-files))))))

(install-record-package-a)
(install-record-package-b)
