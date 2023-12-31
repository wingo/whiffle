(define-syntax-rule (assert! expr)
  (or expr (error "check failed" 'expr)))
(define-syntax-rule (assert-eq! a b)
  (let ((a' a) (b' b))
    (unless (eq? a' b')
      (error "not equal: " 'a "=" a' ", " 'b "=" b'))))

(define-syntax list
  (syntax-rules ()
    ((_) '())
    ((_ head . tail) (cons head (list . tail)))))

(define (pk* vals)
  (<< "pk: " vals)
  (let lp ((vals vals))
    (if (null? (cdr vals))
        (car vals)
        (lp (cdr vals)))))
(define-syntax-rule (pk expr ...)
  (pk* (list expr ...)))

(define (ephemeron-chain-length chain key-box)
  (let lp ((head (ephemeron-table-ref chain 0))
           (key (box-ref key-box))
           (len 0))
    (cond
     ((not head) len)
     (else
      (assert-eq! key (ephemeron-key head))
      (let ((value (assert! (ephemeron-value head))))
        (lp (ephemeron-next head)
            (box-ref value)
            (1+ len)))))))

(define (make-ephemeron-chain length head-key-box)
  (let ((table (make-ephemeron-table 1)))
    (define (make-key x) (cons x x))
    (let lp ((head #f) (i 0))
      (if (< i length)
          (let* ((tail-box (box (box-ref head-key-box)))
                 (next-head-key (make-key i))
                 (next-head (make-ephemeron next-head-key tail-box)))
            (ephemeron-table-push! table 0 next-head)
            (box-set! head-key-box next-head-key)
            (lp next-head (1+ i)))
          table))))

(define-syntax-rule (when-precise-gc expr ...)
  (cond-expand
   (precise-gc (begin expr ...))
   (else #t)))

(define (run-test i chain-length)
  (<< i ": Allocating ephemeron list " chain-length " nodes long.\n")
  (let* ((start (current-microseconds))
         (head-key-box (box #f))
         (chain (make-ephemeron-chain chain-length head-key-box)))
    (assert-eq! (ephemeron-chain-length chain head-key-box)
                chain-length)
    (gc-collect)
    (assert-eq! (ephemeron-chain-length chain head-key-box) chain-length)
    (when-precise-gc
     (box-set! head-key-box #f)
     ;; Assume that gc-collect forces major collections, otherwise the
     ;; key may still be live.
     (gc-collect)
     (assert-eq! (ephemeron-chain-length chain head-key-box) 0))
    (print-elapsed "thread" start)))

(lambda (nthreads chain-length)
  (parallel nthreads
            (lambda (i)
              (run-test i chain-length))))
