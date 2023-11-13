(define (compile expr cenv)
  (define (lookup var cenv k)
    (let lp ((depth 0) (cenv cenv))
      (match cenv
        (() (error "unbound var" var))
        ((rib . cenv)
         (match (list-index rib var)
           (#f (lp (1+ depth) cenv))
           (offset
            (k depth offset)))))))
  (define (valid-bindings? bindings)
    (match bindings
      (() #t)
      ((((? symbol?) _) . bindings) (valid-bindings? bindings))
      (_ #f)))
  (define (binding-var binding) (car binding))
  (define (binding-val binding) (car (cdr binding)))
  (match expr
    ((? symbol? var)
     (lookup var cenv
             (lambda (depth offset)
               (lambda (env)
                 (let lp ((depth depth) (env env))
                   (if (zero? depth)
                       (vector-ref (car env) offset)
                       (lp (1- depth) (cdr env))))))))

    (('set! var val)
     (let ((val (compile val cenv)))
       (lookup var cenv
               (lambda (depth offset)
                 (lambda (env)
                   (let lp ((depth depth) (env env))
                     (if (zero? depth)
                         (vector-set! (car env) offset (val env))
                         (lp (1- depth) (cdr env)))))))))

    (('if test consequent alternate)
     (let ((test (compile test cenv))
           (consequent (compile consequent cenv))
           (alternate (compile alternate cenv)))
       (lambda (env)
         (if (test env)
             (consequent env)
             (alternate env)))))

    (('begin expr) (compile expr cenv))
    (('begin head . tail)
     (let ((head (compile head cenv))
           (tail (compile `(begin . ,tail) cenv)))
       (lambda (env) (head env) (tail env))))

    (('lambda args . body)
     (let ((body (compile `(begin . ,body) (cons args cenv))))
       (define-syntax-rule (args-case pat ...)
         (match args
           (pat (lambda (env)
                  (lambda pat (body (cons (vector . pat) env)))))
           ...))
       (args-case () (a) (a b) (a b c) (a b c d))))

    (('let (? symbol? lp) (? valid-bindings? bindings) . body)
     (let ((vars (map binding-var bindings))
           (vals (map binding-val bindings)))
       (compile `(letrec ((,lp (lambda ,vars . ,body))) (,lp . ,vals)) cenv)))
    (('let (? valid-bindings? bindings) . body)
     (let ((vars (map binding-var bindings))
           (vals (map binding-val bindings)))
       (compile `((lambda ,vars . ,body) . ,vals) cenv)))

    (('letrec (? valid-bindings? bindings) . body)
     (let ((vars (map binding-var bindings))
           (vals (map binding-val bindings)))
       (compile `(let ,(map (lambda (var) `(,var #f)) vars)
                   . ,(let lp ((vars vars) (vals vals) (out body))
                        (match vars
                          (() out)
                          ((var . vars)
                           (match vals
                             ((val . vals)
                              (lp vars vals (cons `(set! ,var ,val) out))))))))
                cenv)))

    (('quote x)
     (lambda (env) x))
    ((or (? exact-integer?) (? string?) #t #f)
     (lambda (env) expr))

    ((proc . args)
     (let ((proc (compile proc cenv))
           (args (map (lambda (arg) (compile arg cenv)) args)))
       (define-syntax-rule (args-case pat ...)
         (match args
           (pat (lambda (env)
                  (define-syntax-rule (call x y (... ...))
                    ((x env) (y env) (... ...)))
                  (call proc . pat)))
           ...))
       (args-case () (a) (a b) (a b c) (a b c d))))))

(define-syntax-rule (define-globals global-names global-values g ...)
  (begin
    (define global-names '(g ...))
    (define global-values (vector g ...))))

(define-globals global-names global-values
  pair? cons car cdr set-car! set-cdr!
  
  exact-integer?
  + - * < <= = > >= 1+ 1- zero? negative?
  quotient remainder
  
  char? char->integer integer->char
  
  eq?
  symbol?

  vector? make-vector vector-length vector-ref vector-set! vector->list
  
  null?
  not
  
  map for-each iota list? length)

(define (eval expr)
  (let ((cenv (cons global-names '()))
        (env (cons global-values '())))
    ((compile expr cenv) env)))

(define fib
  (eval '(letrec ((fib (lambda (n)
                         (if (< n 2)
                             1
                             (+ (fib (- n 1)) (fib (- n 2)))))))
           fib)))

(lambda (nthreads arg)
  (writeln (parallel nthreads (lambda (i) (<< "here") (fib arg)))))
