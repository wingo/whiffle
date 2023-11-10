(define (cpstak x y z)

  (define (tak x y z k)
    (if (not (< y x))
        (k z)
        (tak (- x 1)
             y
             z
             (lambda (v1)
               (tak (- y 1)
                    z
                    x
                    (lambda (v2)
                      (tak (- z 1)
                           x
                           y
                           (lambda (v3)
                             (tak v1 v2 v3 k)))))))))

  (<< "starting")
  (let ((v (tak x y z (lambda (a) a))))
    (<< "finished")
    v))

(lambda (nthreads x y z output)
  (let ((ret (make-vector nthreads #f)))
    (parallel nthreads
              (lambda (i)
                (vector-set! ret i (cpstak x y z))))
    (let lp ((i 0))
      (when (< i nthreads)
        (unless (eq? (vector-ref ret i) output)
          (error "unexpected output" i (vector-ref ret i) output))))))
