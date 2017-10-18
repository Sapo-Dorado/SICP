(define (make-table)
  (let ((local-table (list 'table*)))
    (define (lookup keys table)
      (if (null? table)
        #f
        (if (null? keys)
          (if (null? (car table))
            #f
            (car table))
          (let ((sub-table (assoc (car keys) table)))
            (if sub-table
              (lookup (cdr keys) (cdr sub-table))
              #f)))))

    (define (insert! keys value table)

      (define (create-branch keys value table)
        (define (iter key-list)
          (if (null? (cdr key-list))
            (cons (car key-list) (cons value '()))
            (cons (car key-list) (cons '() (iter (cdr key-list))))))
        (set-cdr! table (cons (iter keys) (cdr table))))

      (if (null? table)
        (create-branch keys value local-table)
        (if (null? keys)
          (set-car! table value)
          (let ((sub-table (assoc (car keys) table)))
            (if subtable
              (insert! (cdr keys) value (cdr sub-table))
              (create-branch keys value table)))))
      (print-table))

    (define (print-table)
      (newline)
      (display local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup) (lambda (keys) (lookup keys (cdr local-table))))
            ((eq? m 'insert!) (lambda (keys value) (insert! keys value (cdr local-table))))
            ((eq? m 'print) (print-table))
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define (create-branch keys value table)
  (define (iter key-list)
    (if (null? (cdr key-list))
      (cons (car key-list) (cons value '()))
      (cons (car key-list) (cons '() (iter (cdr key-list))))))
  (set-cdr! table (cons (iter keys) (cdr table))))


(define table (list 'table))

(create-branch (list 'a 'b) 'c table)

table
