(define-syntax-rule (simple-match e cs ...)
  (let ((v e)) (simple-match-1 v cs ...)))

(define-syntax simple-match-1
  (syntax-rules ()
    ((_ v) (error "value failed to match" v))
    ((_ v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (simple-match-1 v cs ...))))
       (simple-match-pat v pat (let () e0 e ...) (fk))))))

(define-syntax simple-match-pat
  (syntax-rules (_ quote unquote ? and or not)
    ((_ v _ kt kf) kt)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v #t kt kf) (if (eq? v #t) kt kf))
    ((_ v #f kt kf) (if (eq? v #f) kt kf))
    ((_ v (and) kt kf) kt)
    ((_ v (and x . y) kt kf)
     (simple-match-pat v x (simple-match-pat v (and . y) kt kf) kf))
    ((_ v (or) kt kf) kf)
    ((_ v (or x . y) kt kf)
     (let ((tk (lambda () kt)))
       (simple-match-pat v x (tk) (simple-match-pat v (or . y) (tk) kf))))
    ((_ v (not pat) kt kf) (simple-match-pat v pat kf kt))
    ((_ v (quote lit) kt kf)
     (if (eq? v (quote lit)) kt kf))
    ((_ v (? proc) kt kf) (simple-match-pat v (? proc _) kt kf))
    ((_ v (? proc pat) kt kf)
     (if (proc v) (simple-match-pat v pat kt kf) kf))
    ((_ v (x . y) kt kf)
     (if (pair? v)
         (let ((vx (car v)) (vy (cdr v)))
           (simple-match-pat vx x (simple-match-pat vy y kt kf) kf))
         kf))
    ((_ v #(x ...) kt kf)
     (if (and (vector? v)
              (eq? (vector-length v) (length '(x ...))))
         (let ((vv (vector->list v)))
           (simple-match-pat vv (x ...) kt kf))
         kf))
    ((_ v var kt kf) (let ((var v)) kt))))

(define-syntax-rule (match e cs ...) (simple-match e cs ...))

(define (pair? x) (%pair? x))
(define (cons x y) (%cons x y))
(define (car x) (%car x))
(define (cdr x) (%cdr x))
(define (set-car! x y) (%set-car! x y))
(define (set-cdr! x y) (%set-cdr! x y))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (caar x)))
(define (cadar x) (car (cdar x)))
(define (caadr x) (car (cadr x)))
(define (caddr x) (car (cddr x)))
(define (cdaar x) (cdr (caar x)))
(define (cddar x) (cdr (cdar x)))
(define (cdadr x) (cdr (cadr x)))
(define (cdddr x) (cdr (cddr x)))

(define (caaaar x) (car (caaar x)))
(define (caadar x) (car (cadar x)))
(define (caaadr x) (car (caadr x)))
(define (caaddr x) (car (caddr x)))
(define (cdaaar x) (cdr (caaar x)))
(define (cdadar x) (cdr (cadar x)))
(define (cdaadr x) (cdr (caadr x)))
(define (cdaddr x) (cdr (caddr x)))
(define (cadaar x) (car (cdaar x)))
(define (caddar x) (car (cddar x)))
(define (cadadr x) (car (cdadr x)))
(define (cadddr x) (car (cdddr x)))
(define (cddaar x) (cdr (cdaar x)))
(define (cdddar x) (cdr (cddar x)))
(define (cddadr x) (cdr (cdadr x)))
(define (cddddr x) (cdr (cdddr x)))

(define (acons x y tail) (cons (cons x y) tail))

(define (exact-integer? x) (%exact-integer? x))
(define (number? x) (exact-integer? x))
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
(define (string->vector str)
  (call-c-primitive/result "vm_string_to_vector" str))
(define (vector->string v)
  (call-c-primitive/alloc "vm_vector_to_string" v))
(define (string->list str)
  (vector->list (string->vector str)))
(define (string-append a b)
  (vector->string (vector-append (string->vector a) (string->vector b))))

(define (number->string x)
  (let ((digits (let lp ((x (if (< x 0) (- 0 x) x)) (out '()))
                  (if (zero? x)
                      out
                      (lp (quotient x 10)
                          (cons (integer->char
                                 (+ (char->integer #\0) (remainder x 10)))
                                out))))))
    (if (null? digits)
        "0"
        (vector->string (list->vector (if (< x 0) (cons #\- digits) digits))))))

(define (symbol? x) (%symbol? x))
(define (symbol->string x) (%symbol->string x))
(define (string->symbol x) (%string->symbol x))

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
(define (vector-append a b)
  (let* ((len (+ (vector-length a) (vector-length b)))
         (v (make-vector len #f)))
    (let lp ((i 0))
      (if (< i (vector-length a))
          (begin
            (vector-set! v i (vector-ref a i))
            (lp (1+ i)))
          (let lp ((j 0))
            (if (< j (vector-length b))
                (begin
                  (vector-set! v (+ i j) (vector-ref b j))
                  (lp (1+ j)))
                v))))))

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

(define-syntax list
  (syntax-rules ()
    ((list) '())
    ((list a . b) (cons a (list . b)))))
(define (list? x)
  (or (null? x)
      (and (pair? x)
           (list? (cdr x)))))
(define (length x)
  (let lp ((x x) (len 0))
    (if (null? x)
        len
        (lp (cdr x) (1+ len)))))
(define (list-index l x)
  (let lp ((l l) (idx 0))
    (cond
     ((null? l) #f)
     ((eq? x (car l)) idx)
     (else (lp (cdr l) (1+ idx))))))
(define (list-ref l i)
  (if (zero? i)
      (car l)
      (list-ref (cdr l) (1- i))))

(define (equal? x y)
  (cond
   ((eq? x y))
   ((pair? x)
    (and (pair? y)
         (equal? (car x) (car y))
         (equal? (cdr x) (cdr y))))
   ((vector? x)
    (and (vector? y)
         (eq? (vector-length x) (vector-length y))
         (let lp ((i 0))
           (or (= i (vector-length x))
               (and (equal? (vector-ref x i) (vector-ref y i))
                    (lp (1+ i)))))))
   ((string? x)
    (and (string? y)
         (equal? (string->vector x) (string->vector y))))
   ((bytevector? x)
    (and (bytevector? y)
         (eq? (bytevector-length x) (bytevector-length y))
         (let lp ((i 0))
           (or (= i (bytevector-length x))
               (and (eq? (bytevector-u8-ref x i)
                         (bytevector-u8-ref y i))
                    (lp (1+ i)))))))
   ((box? x)
    (and (box? y)
         (equal? (box-ref x) (box-ref y))))
   (else #f)))

(define (assq x alist)
  (let lp ((alist alist))
    (match alist
      (() #f)
      (((and pair (k . v)) . alist)
       (if (eq? k x) pair (lp alist))))))
(define (memq x l)
  (let lp ((l l))
    (match l
      (() #f)
      ((head . tail)
       (if (eq? x head) l (lp tail))))))

(define (member x l)
  (let lp ((l l))
    (match l
      (() #f)
      ((head . tail)
       (if (equal? x head) l (lp tail))))))

(define (vector->list v)
  (let lp ((i 0))
    (if (< i (vector-length v))
        (cons (vector-ref v i) (lp (1+ i)))
        '())))
(define (list->vector l)
  (let ((v (make-vector (length l) #f)))
    (let lp ((l l) (i 0))
      (match l
        (() v)
        ((head . tail)
         (vector-set! v i head)
         (lp tail (1+ i)))))))
(define (reverse l)
  (let lp ((l l) (out '()))
    (if (null? l)
        out
        (lp (cdr l) (cons (car l) out)))))
(define (append a b)
  (match a
    (() b)
    ((x . a) (append a (cons x b)))))


(define (make-bytevector size init)
  (call-c-primitive/alloc "vm_make_bytevector" size init))

(define (bytevector? bv) (%bytevector? bv))
(define (bytevector-length bv) (%bytevector-length bv))
(define (bytevector-u8-ref bv i) (%bytevector-u8-ref bv i))
(define (bytevector-u8-set! bv i u8) (%bytevector-u8-set! bv i u8))

(define (random-fixnum!)
  (call-c-primitive/thread "thread_random_fixnum"))

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
      (recur (- 0 x)))
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
   ((char? x)
    (write-char #\#)
    (write-char #\\)
    (write-char x))
   ((pair? x)
    (cond
     ((and (eq? (car x) 'quote)
           (pair? (cdr x))
           (null? (cddr x)))
      (write-char #\')
      (recur (cadr x)))
     (else
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
          (write-char #\))))))))
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
  (call-c-primitive/write "vm_ephemeron_table_push" v i e))

(define (current-microseconds)
  (call-c-primitive/result "vm_current_microseconds"))

(define-syntax-rule (<< expr ...)
  (begin (display expr) ... (newline)))

(define-syntax-rule (error expr ...)
  (begin
    (<< expr ...)
    (call-c-primitive "vm_die")))

(define (parallel n f)
  (let ((results (make-vector n #f)))
    (let ((threads (let lp ((i 1))
                     (if (< i n)
                         (cons (spawn-thread
                                (lambda ()
                                  (vector-set! results i (f i))))
                               (lp (1+ i)))
                         '()))))
      (vector-set! results 0 (f 0))
      (for-each join-thread threads)
      results)))

(define (print-elapsed what start)
  (let ((elapsed (- (current-microseconds) start)))
    (<< what ": completed in " elapsed " usec.")))

(define (gc-collect)
  (call-c-primitive/alloc "vm_gc_collect"))
