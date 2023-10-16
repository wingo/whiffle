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
(define (negative? x) (< x 0))
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

(define (make-bytevector size init)
  (call-c-primitive/alloc "vm_make_bytevector" size init))

(define (bytevector? bv) (%bytevector? bv))
(define (bytevector-length bv) (%bytevector-length bv))
(define (bytevector-u8-ref bv i) (%bytevector-u8-ref bv i))
(define (bytevector-u8-set! bv i u8) (%bytevector-u8-set! bv i u8))

(define (box val) (%box val))
(define (box? x) (%box? x))
(define (box-ref x) (%box-ref x))
(define (box-set! x val) (%box-set! x val))

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
   ((not x)
    (write-char #\#)
    (write-char #\f))
   ((eq? x #t)
    (write-char #\#)
    (write-char #\t))
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
        (recur tail)
        (write-char #\))))))
   ((string? x)
    (cond
     (quote-strings?
      (write-char #\")
      (for-each (lambda (ch)
                  (when (or (eq? ch #\") (eq? ch #\\))
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
   ((bytevector? x)
    (write-char #\#)
    (write-char #\v)
    (write-char #\u)
    (write-char #\8)
    (write-char #\()
    (let lp ((i 0))
      (when (< i (bytevector-length x))
        (unless (zero? i)
          (write-char #\space))
        (recur (bytevector-u8-ref x i))
        (lp (1+ i))))
    (write-char #\)))
   ((ephemeron? x)
    (cond
     ((ephemeron-key x)
      => (lambda (k)
           (print "#<ephemeron " #f)
           (recur k)
           (write-char #\:)
           (write-char #\space)
           (recur (ephemeron-value x))
           (write-char #\>)))
     (else
      (print "#<dead-ephemeron>" #f))))
   ((ephemeron-table? x)
    (print "#<ephemeron-table>" #f))
   (else
    (recur "unhandled object :("))))

(define (write x)   (print x #t))
(define (display x) (print x #f))

(define (writeln x)   (write x)   (newline))
(define (displayln x) (display x) (newline))

(define (spawn-thread thunk)
  (call-c-primitive/alloc "vm_spawn_thread" thunk))

(define (join-thread thread)
  (call-c-primitive/alloc "vm_join_thread" thread))

(define (make-ephemeron key value)
  (call-c-primitive/alloc "vm_make_ephemeron" key value))
(define (ephemeron? x)
  (call-c-primitive/pred "vm_is_ephemeron" x))
(define (ephemeron-key x)
  (call-c-primitive/result "vm_ephemeron_key" x))
(define (ephemeron-value x)
  (call-c-primitive/result "vm_ephemeron_value" x))
(define (ephemeron-next x)
  (call-c-primitive/result "vm_ephemeron_next" x))
(define (ephemeron-kill! x)
  (call-c-primitive "vm_ephemeron_kill" x))

(define (make-ephemeron-table size)
  (call-c-primitive/alloc "vm_make_ephemeron_table" size))
(define (ephemeron-table? x)
  (call-c-primitive/pred "vm_is_ephemeron_table" x))
(define (ephemeron-table-length x)
  (call-c-primitive/result "vm_ephemeron_table_length" x))
(define (ephemeron-table-ref v i)
  (call-c-primitive/result "vm_ephemeron_table_ref" v i))
(define (ephemeron-table-push! v i e)
  (call-c-primitive "vm_ephemeron_table_push" v i e))

(define (current-microseconds)
  (call-c-primitive/result "vm_current_microseconds"))

(define-syntax-rule (<< expr ...)
  (begin (display expr) ... (newline)))

(define-syntax-rule (error expr ...)
  (begin
    (<< expr ...)
    (call-c-primitive "vm_die")))

(define (parallel n f)
  (let ((threads (let lp ((i 1))
                   (if (< i n)
                       (cons (spawn-thread (lambda () (f i)))
                             (lp (1+ i)))
                       '()))))
    (f 0)
    (for-each join-thread threads)))

(define (print-elapsed what start)
  (let ((elapsed (- (current-microseconds) start)))
    (<< what ": completed in " elapsed " usec.")))

(define (gc-collect)
  (call-c-primitive/alloc "vm_gc_collect"))
