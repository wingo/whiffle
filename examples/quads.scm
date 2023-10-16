(define-syntax-rule (assert! expr)
  (unless expr
    (error "check failed" 'expr)))

(define (make-quad a b c d) (vector a b c d))
(define (quad-a quad) (vector-ref quad 0))
(define (quad-b quad) (vector-ref quad 1))
(define (quad-c quad) (vector-ref quad 2))
(define (quad-d quad) (vector-ref quad 3))

;; Build tree bottom-up
(define (make-tree depth)
  (and (<= 0 depth)
       (make-quad (make-tree (1- depth))
                  (make-tree (1- depth))
                  (make-tree (1- depth))
                  (make-tree (1- depth)))))

(define (validate-tree tree depth)
  (cond
   ((negative? depth) (assert! (not tree)))
   (else
    (assert! tree)
    (validate-tree (quad-a tree) (1- depth))
    (validate-tree (quad-b tree) (1- depth))
    (validate-tree (quad-c tree) (1- depth))
    (validate-tree (quad-d tree) (1- depth)))))

(define (tree-size depth)
  (if (zero? depth)
      1
      (+ 1 (* (tree-size (1- depth)) 4))))

(define (run-test i depth)
  (<< i ": Making quad tree of depth " depth " (" (tree-size depth) " nodes).")
  (let* ((start (current-microseconds))
         (long-lived-tree (make-tree depth)))
    (print-elapsed "construction" start)
    (validate-tree long-lived-tree depth)
    (<< i "Allocating garbage tree of depth " (1- depth)
        " (" (tree-size (1- depth)) " nodes), 60 times,"
        " validating live tree each time.")
    (let ((alloc-start (current-microseconds)))
      (let lp ((n 0))
        (when (< n 60)
          (make-tree (1- depth))
          (validate-tree long-lived-tree depth)
          (lp (1+ n))))
      (print-elapsed "allocation loop" alloc-start))
    (print-elapsed "quads test" start)))

(lambda (nthreads depth)
  (parallel nthreads
            (lambda (i)
              (run-test i depth))))
