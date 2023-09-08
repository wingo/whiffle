(define (pair? x) (%pair? x))
(define (cons x y) (%cons x y))
(define (car x) (%car x))
(define (cdr x) (%cdr x))
(define (set-car! x y) (%set-car! x y))
(define (set-cdr! x y) (%set-cdr! x y))

(define (exact-integer? x) (%exact-integer? x))
(define (+ x y) (%+ x y))
(define (- x y) (%- x y))
(define (* x y) (%* x y))
(define (< x y) (%< x y))
(define (<= x y) (%<= x y))
(define (= x y) (%= x y))
(define (> x y) (%> x y))
(define (>= x y) (%>= x y))
(define (quotient x y) (%quotient x y))
(define (remainder x y) (%remainder x y))

(define (char? x) (%char? x))
(define (char->integer x) (%char->integer x))
(define (integer->char x) (%integer->char x))

(define (eq? x y) (%eq? x y))
(define (eqv? x y) (%eqv? x y))

(define (string? x) (%string? x))

(define (symbol? x) (%symbol? x))
(define (symbol->string x) (%symbol->string x))

(define (vector? x) (%vector? x))
(define (make-vector x init) (%make-vector x init))
(define-syntax vector
  (lambda (stx)
    (syntax-case stx ()
      ((_ . args) #'(%vector . args))
      (id (identifier? #'id) (lambda args (list->vector args))))))
(define (vector-length x) (%vector-length x))
(define (vector-ref x i) (%vector-ref x i))
(define (vector-set! x i v) (%vector-set! x i v))

(define (null? x) (eq? x '()))
(define (1+ x) (+ x 1))
(define (1- x) (- x 1))
(define (zero? x) (eq? x 0))
(define (not x) (if x #f #t))

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (map f (cdr l)))))

(define (for-each f l)
  (unless (null? l)
    (f (car l))
    (for-each f (cdr l))))

(define (iota n)
  (let lp ((i 0))
    (if (eq? i n)
        '()
        (cons i (lp (1+ i))))))

(define (list? x)
  (or (null? x)
      (and (pair? x)
           (list? (cdr x)))))

(define (vector->list v)
  (let lp ((i 0))
    (if (< i (vector-length v))
        (cons (vector-ref v i) (lp (1+ i)))
        '())))

(define (string->vector str)
  (call-c-primitive/result "vm_string_to_vector" str))

(define (string->list str)
  (vector->list (string->vector str)))

(define (write-char ch)
  (call-c-primitive "vm_write_char" ch))

(define (newline)
  (write-char #\newline))

(define (print x quote-strings?)
  (define (recur x) (print x quote-strings?))
  (cond
   ((null? x)
    (write-char #\()
    (write-char #\)))
   ((exact-integer? x)
    (cond
     ((< x 0)
      (write-char #\-)
      (recur (- x)))
     ((= x 0)
      (write-char #\0))
     (else
      (let ((digits (let lp ((x x) (out '()))
                      (if (zero? x)
                          out
                          (lp (quotient x 10)
                              (cons (remainder x 10) out))))))
        (for-each (lambda (n)
                    (write-char (integer->char (+ (char->integer #\0) n))))
                  digits)))))
   ((pair? x)
    (write-char #\()
    (recur (car x))
    (let lp ((tail (cdr x)))
      (cond
       ((null? tail)
        (write-char #\)))
       ((pair? tail)
        (write-char #\space)
        (recur (car tail))
        (lp (cdr tail)))
       (else
        (write-char #\space)
        (write-char #\.)
        (write-char #\space)
        (recur (cdr tail))
        (write-char #\))))))
   ((string? x)
    (cond
     (quote-strings?
      (write-char #\")
      (for-each (lambda (ch)
                  (when (eq? ch #\")
                    (write-char #\\))
                  (write-char ch))
                (string->list x))
      (write-char #\"))
     (else
      (for-each write-char (string->list x)))))
   ((symbol? x)
    (print (symbol->string x) #f))
   ((vector? x)
    (write-char #\#)
    (write-char #\()
    (recur (vector->list x))
    (write-char #\)))
   (else
    (recur "unhandled object :("))))

(define (write x)   (print x #t))
(define (display x) (print x #f))

(define (writeln x)   (write x)   (newline))
(define (displayln x) (display x) (newline))

(define (spawn-thread thunk)
  ;; make thread object, with space for join val
  ;; spawn thread with tid
  ;; set tid in thread object
  (call-c-primitive/alloc "vm_spawn_thread" thunk))

(define (join-thread thread)
  (call-c-primitive/alloc "vm_join_thread" thread))
