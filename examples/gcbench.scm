(define min-tree-depth 4)
(define max-tree-depth 16)
(define long-lived-bytevector-size #e4e6)
(define long-lived-tree-depth 16)

(define (make-node left right i j) (vector left right i j))
(define (make-empty-node) (make-node #f #f 0 0))
(define (node-left node) (vector-ref node 0))
(define (node-right node) (vector-ref node 1))
(define (node-i node) (vector-ref node 2))
(define (node-j node) (vector-ref node 3))
(define (set-left! node left) (vector-set! node 0 left))
(define (set-right! node right) (vector-set! node 1 right))
(define (set-j! node j) (vector-set! node 3 j))

(define (make-power-law-distribution)
  (define counter 0)
  ;; A power-law distribution.  Each integer was selected by starting at
  ;; 0, taking a random number in [0,1), and then accepting the integer
  ;; if the random number was less than 0.15, or trying again with the
  ;; next integer otherwise.  Useful for modelling allocation sizes or
  ;; number of garbage objects to allocate between live allocations.
  (define values
    #vu8(1 15 3 12 2 8 4 0 18 7 9 8 15 2 36 5
         1 9 6 11 9 19 2 0 0 3 9 6 3 2 1 1
         6 1 8 4 2 0 5 3 7 0 0 3 0 4 1 7
         1 8 2 2 2 14 0 7 8 0 2 1 4 12 7 5
         0 3 4 13 10 2 3 7 0 8 0 23 0 16 1 1
         6 28 1 18 0 3 6 5 8 6 14 5 2 5 0 11
         0 18 4 16 1 4 3 13 3 23 7 4 10 5 3 13
         0 14 5 5 2 5 0 16 2 0 1 1 0 0 4 2
         7 7 0 5 7 2 1 24 27 3 7 1 0 8 1 4
         0 3 0 7 7 3 9 2 9 2 5 10 1 1 12 6
         2 9 5 0 4 6 0 7 2 1 5 4 1 0 1 15
         4 0 15 4 0 0 32 18 2 2 1 7 8 3 11 1
         2 7 11 1 9 1 2 6 11 17 1 2 5 1 14 3
         6 1 1 15 3 1 0 6 10 8 1 3 2 7 0 1
         0 11 3 3 5 8 2 0 0 7 12 2 5 20 3 7
         4 4 5 22 1 5 2 7 15 2 4 6 11 8 12 1))

  (lambda ()
    (let* ((i counter)
           (value (bytevector-u8-ref values i))
           (i* (1+ i)))
      (set! counter (if (eq? i* (bytevector-length values)) 0 i*))
      value)))

(define (validate-tree! node depth)
  (cond-expand
   (check-heap-consistency
    (unless (eq? (node-i node) 0) (error "bad i"))
    (unless (eq? (node-j node) depth) (error "bad j"))
    (cond
     ((zero? depth)
      (when (node-left node) (error "unexpected left"))
      (when (node-right node) (error "unexpected right")))
     (else
      (validate-tree! (node-left node) (1- depth))
      (validate-tree! (node-right node) (1- depth)))))
   (else
    #t)))

(define (tree-size depth)
  (if (zero? depth)
      1
      (+ 1 (* 2 (tree-size (1- depth))))))
(define (compute-num-iters depth)
  (quotient (* 2 (tree-size (+ max-tree-depth 2)))
            (tree-size depth)))

(define (run-test i)
  (define garbage-size (make-power-law-distribution))
  (define (allocate-garbage!)
    (let ((size (garbage-size)))
      (unless (zero? size) (make-vector (1- size) #f))))
  (define (make-tree depth)
    (and (<= 0 depth)
         (let ()
           (define left (make-tree (1- depth)))
           (define right (make-tree (1- depth)))
           (allocate-garbage!)
           (make-node left right 0 depth))))
  (define (populate-node! depth node)
    (when (< 0 depth)
      (allocate-garbage!)
      (define left (make-empty-node))
      (allocate-garbage!)
      (define right (make-empty-node))
      (set-left! node left)
      (set-right! node right)
      (set-j! node depth)
      (populate-node! (1- depth) left)
      (populate-node! (1- depth) right)))

  (define (time-construction depth)
    (define iterations (compute-num-iters depth))

    (<< i ": creating " iterations " trees of depth " depth)

    (let ((start (current-microseconds)))
      (let lp ((iterations iterations))
        (unless (zero? iterations)
          (let ((tmp (make-empty-node)))
            (populate-node! depth tmp)
            (validate-tree! tmp depth)
            (lp (1- iterations)))))
      (print-elapsed "top-down construction" start))
    
    (let ((start (current-microseconds)))
      (let lp ((iterations iterations))
        (unless (zero? iterations)
          (let ((tmp (make-tree depth)))
            (validate-tree! tmp depth)
            (lp (1- iterations)))))
      (print-elapsed "bottom-up construction" start)))

  (<< i ": creating long-lived binary tree of depth " long-lived-tree-depth)

  (define long-lived-tree (make-tree long-lived-tree-depth))
  (define long-lived-bytevector (make-bytevector long-lived-bytevector-size 0))
  (let lp ((i 0) (v 0))
    (when (< i long-lived-bytevector-size)
      (bytevector-u8-set! long-lived-bytevector i v)
      (lp (1+ i) (if (eq? v #xff) 0 (1+ v)))))
  (let lp ((i min-tree-depth))
    (when (<= i max-tree-depth)
      (time-construction i)
      (lp (+ i 2))))
  (validate-tree! long-lived-tree long-lived-tree-depth)
  ;; These references keep long-lived-tree and long-lived-bytevector
  ;; alive.
  (unless (eq? (node-i long-lived-tree) 0)
    (error "long-lived tree has unexpected value"))
  (unless (eq? (bytevector-u8-ref long-lived-bytevector 1000)
               (remainder 1000 256))
    (error "long-lived bytevector has unexpected contents"))
  #t)

(lambda (nthreads)
  (parallel nthreads (lambda (i) (run-test i))))
