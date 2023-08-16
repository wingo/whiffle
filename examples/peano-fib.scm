(lambda (n)
  (define (make-peano n)
    (if (zero? n)
        '()
        (cons 0 (make-peano (1- n)))))
  (define (peano-value n)
    (if (null? n)
        0
        (1+ (peano-value (cdr n)))))
  (let ()
    (define (zero? n) (null? n))
    (define zero '())
    (define (1- n) (cdr n))
    (define (1+ n) (cons 0 n))
    (define (+ x y)
      (if (zero? x)
          y
          (1+ (+ (1- x) y))))
    (define (- x y)
      (if (zero? y)
          x
          (- (1- x) (1- y))))
    (define (< x y)
      (cond
       ((zero? x) (not (zero? y)))
       ((zero? y) #f)
       (else (< (1- x) (1- y)))))

    (define (fib n)
      (if (< n (1+ (1+ zero)))
          (1+ zero)
          (+ (fib (1- n)) (fib (1- (1- n))))))

    (peano-value (fib (make-peano n)))))
