(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (if (eq? frame the-empty-frame)
        (error "no var to unbind in this frame" frame)
        (if (eq? (frame-first-var (rest-frame frame)) var)
          (set-cdr! frame (cddr frame))
          (scan (cdr frame)))))
    (if (eq? var (frame-first-var frame))
      (set-car! frame (cdr frame))
      (scan frame))))