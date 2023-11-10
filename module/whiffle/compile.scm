;;; Lightweight Scheme compiler directly to C.
;;; Copyright (C) 2023 Andy Wingo.

;;; Derived from (language tree-il compile-bytecode) in Guile, which is:
;;; Copyright (C) 2020, 2021 Free Software Foundation, Inc.

;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (whiffle compile)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (language tree-il)
  #:use-module ((language tree-il optimize) #:select (make-lowerer))
  #:use-module (rnrs bytevectors)
  #:use-module ((srfi srfi-1) #:select (fold
                                        fold-right
                                        lset-adjoin lset-union lset-difference))
  #:use-module (srfi srfi-9)
  #:export (compile-to-c))

(define-record-type <static-closure>
  (make-static-closure label)
  static-closure?
  (label static-closure-label))

(define-record-type <asm>
  (make-asm decl-port code-port constants next-constant next-label)
  asm?
  (decl-port asm-decl-port)
  (code-port asm-code-port)
  (constants asm-constants)
  (next-constant asm-next-constant set-asm-next-constant!)
  (next-label asm-next-label set-asm-next-label!))

(define-syntax-rule (<-code asm fmt arg ...)
  (format (asm-code-port asm) fmt arg ...))
(define-syntax-rule (<-decl asm fmt arg ...)
  (format (asm-decl-port asm) fmt arg ...))

(define (emit-begin-function asm label)
  (<-decl asm "static VM F~a (VM vm, size_t nargs);\n"
          label)
  (<-code asm "static VM F~a (VM vm, size_t nargs) {\n"
          label))

(define (emit-end-function asm)
  (<-code asm "}\n\n"))

(define (emit-assert-nargs-= asm nargs)
  (<-code asm "  if (nargs != ~a) vm_wrong_num_args(~a, nargs);\n" nargs nargs))

(define (emit-expand-sp asm slots)
  (unless (zero? slots)
    (<-code asm "  vm_check_stack(vm, ~a);\n" slots)
    (<-code asm "  vm = vm_expand_stack(vm, ~a);\n" slots)))

(define (make-label asm)
  (let ((label (asm-next-label asm)))
    (set-asm-next-label! asm (1+ label))
    label))
(define (emit-bind-label asm label)
  (<-code asm "L~a:\n" label))

(define (constant-code asm const)
  (define (static-code idx)
    (format #f "STATIC_CODE (C~a)" idx))
  (define (intern!)
    (let ((idx (asm-next-constant asm)))
      (hash-set! (asm-constants asm) const idx)
      (set-asm-next-constant! asm (1+ idx))
      idx))
  (define (intern-new-constant)
    (match const
      ((a . b)
       (let* ((car (constant-code asm a))
              (cdr (constant-code asm b))
              (idx (intern!)))
         (<-decl asm
                 "static const Pair C~a = STATIC_PAIR (~a, ~a);\n"
                 idx car cdr)
         idx))
      (#(vals ...)
       (let* ((vals (map (lambda (val) (constant-code asm val)) vals))
              (idx (intern!)))
         (<-decl asm
                 "static const Vector C~a = STATIC_VECTOR (~a~{, {~a}~});\n"
                 idx (length vals) vals)
         idx))
      ((? string?)
       (let* ((v (constant-code asm (list->vector (string->list const))))
              (idx (intern!)))
         (<-decl asm
                 "static const String C~a = STATIC_STRING (~a);\n"
                 idx v)
         idx))
      ((? symbol?)
       (let* ((v (constant-code asm (symbol->string const)))
              (idx (intern!)))
         (<-decl asm
                 "static const Symbol C~a = STATIC_SYMBOL (~a);\n"
                 idx v)
         idx))
      ((? bytevector?)
       (let* ((idx (intern!)))
         (<-decl asm
                 "static const Bytevector C~a = STATIC_BYTEVECTOR (~a~{, ~a~});\n"
                 idx (bytevector-length const) (bytevector->u8-list const))
         idx))
      (($ <static-closure> label)
       (let* ((idx (intern!)))
         (<-decl asm "static Closure C~a = STATIC_CLOSURE (F~a);\n" idx label)
         idx))))
  (match const
    ((? integer?) (format #f "IMMEDIATE_INTEGER_CODE (~a)" const))
    ((? char?) (format #f "IMMEDIATE_CHAR_CODE (~a)" (char->integer const)))
    ((? unspecified?) "IMMEDIATE_UNSPECIFIED_CODE")
    (#f "IMMEDIATE_FALSE_CODE")
    (#t "IMMEDIATE_TRUE_CODE")
    (() "IMMEDIATE_NULL_CODE")
    (_ (static-code (or (hash-ref (asm-constants asm) const)
                        (intern-new-constant))))))

(define (constant-ref asm const)
  (format #f "CONST (~a)" (constant-code asm const)))

(define (emit-constant-ref asm dst const)
  (<-code asm "  vm.sp[~a] = ~a;\n" dst (constant-ref asm const)))

(define (emit-make-closure asm dst func-label nfree junk-slots)
  (if (zero? nfree)
      (emit-constant-ref asm dst (make-static-closure func-label))
      (<-code asm
              "  vm.sp[~a] = vm_make_closure(vm_trim(vm, ~a), &F~a, ~a);\n"
              dst junk-slots func-label nfree)))

(define (emit-closure-init/bound asm closure-idx free-idx val)
  (<-code asm
          "  vm_closure_init(vm.sp[~a], ~a, vm.sp[~a]);\n"
          closure-idx free-idx val))

(define (emit-closure-init/free asm closure-idx free-idx self-idx val)
  (<-code asm
          "  vm_closure_init(vm.sp[~a], ~a, vm_closure_ref (vm.sp[~a], ~a));\n"
          closure-idx free-idx self-idx val))

(define (emit-mov asm dst src)
  (<-code asm "  vm.sp[~a] = vm.sp[~a];\n" dst src))

(define (emit-free-ref asm dst self-idx idx)
  (<-code asm "  vm.sp[~a] = vm_closure_ref(vm.sp[~a], ~a);\n"
          dst self-idx idx))

(define (emit-boxed-bound-set asm bound-idx val)
  (<-code asm "  vm_box_set(vm.sp[~a], vm.sp[~a]);\n" bound-idx val))
(define (emit-boxed-free-set asm self-idx free-idx val)
  (<-code asm "  vm_box_set(vm_closure_ref (vm.sp[~a], ~a), vm.sp[~a]);\n"
          self-idx free-idx val))

(define (emit-return asm slots)
  (<-code asm "  return vm_trim(vm, ~a);\n" slots))

(define (emit-tail-call asm frame-size base nargs)
  (for-each (lambda (i)
              (<-code asm "  vm.sp[~a] = vm.sp[~a];\n"
                      (- frame-size i 1)
                      (- base i 1)))
            (iota nargs))
  (<-code asm "  return vm_closure_code(vm.sp[~a])(vm_trim(vm, ~a), ~a);\n"
          (1- frame-size) (- frame-size nargs) nargs))
(define (emit-call asm base nargs)
  (<-code asm "  vm = vm_closure_code(vm.sp[~a])(vm_trim(vm, ~a), ~a);\n"
          (1- base) (- base nargs) nargs))
(define (emit-restore-sp asm slots)
  (<-code asm "  vm = vm_expand_stack(vm, ~a);\n" slots))

(define (emit-add asm dst a b)
  (<-code asm "  vm.sp[~a] = vm_add(vm.sp[~a], vm.sp[~a]);\n" dst a b))
(define (emit-sub asm dst a b)
  (<-code asm "  vm.sp[~a] = vm_sub(vm.sp[~a], vm.sp[~a]);\n" dst a b))
(define (emit-mul asm dst a b)
  (<-code asm "  vm.sp[~a] = vm_mul(vm.sp[~a], vm.sp[~a]);\n" dst a b))
(define (emit-quo asm dst a b)
  (<-code asm "  vm.sp[~a] = vm_quo(vm.sp[~a], vm.sp[~a]);\n" dst a b))
(define (emit-rem asm dst a b)
  (<-code asm "  vm.sp[~a] = vm_rem(vm.sp[~a], vm.sp[~a]);\n" dst a b))
(define (emit-box asm dst src junk-slots)
  (<-code asm "  vm.sp[~a] = vm_box(vm_trim(vm, ~a), &vm.sp[~a]);\n" dst junk-slots src))
(define (emit-box-ref asm dst src)
  (<-code asm "  vm.sp[~a] = vm_box_ref(vm.sp[~a]);\n" dst src))
(define (emit-box-set asm box val)
  (<-code asm "  vm_box_set(vm.sp[~a], vm.sp[~a]);\n" box val))
(define (emit-cons asm dst a b junk-slots)
  (<-code asm "  vm.sp[~a] = vm_cons(vm_trim(vm, ~a), &vm.sp[~a], &vm.sp[~a]);\n" dst junk-slots a b))
(define (emit-car asm dst src)
  (<-code asm "  vm.sp[~a] = vm_car(vm.sp[~a]);\n" dst src))
(define (emit-cdr asm dst src)
  (<-code asm "  vm.sp[~a] = vm_cdr(vm.sp[~a]);\n" dst src))
(define (emit-set-car asm dst src)
  (<-code asm "  vm_set_car(vm.sp[~a], vm.sp[~a]);\n" dst src))
(define (emit-set-cdr asm dst src)
  (<-code asm "  vm_set_cdr(vm.sp[~a], vm.sp[~a]);\n" dst src))
(define (emit-allocate-vector asm dst size junk-slots)
  (<-code asm "  vm.sp[~a] = vm_allocate_vector(vm_trim(vm, ~a), ~a);\n" dst junk-slots size))
(define (emit-vector-init asm v idx val)
  (<-code asm "  vm_vector_init(vm.sp[~a], ~a, vm.sp[~a]);\n" v idx val))
(define (emit-make-vector asm dst size init junk-slots)
  (<-code asm "  vm.sp[~a] = vm_make_vector(vm_trim(vm, ~a), vm.sp[~a], &vm.sp[~a]);\n" dst junk-slots size init))
(define (emit-vector-length asm dst v)
  (<-code asm "  vm.sp[~a] = vm_vector_length(vm.sp[~a]);\n" dst v))
(define (emit-vector-ref asm dst v idx)
  (<-code asm "  vm.sp[~a] = vm_vector_ref(vm.sp[~a], vm.sp[~a]);\n" dst v idx))
(define (emit-vector-set asm v idx val)
  (<-code asm "  vm_vector_set(vm.sp[~a], vm.sp[~a], vm.sp[~a]);\n" v idx val))
(define (emit-string->vector asm dst str)
  (<-code asm "  vm.sp[~a] = vm_string_to_vector(vm.sp[~a]);\n" dst str))
(define (emit-symbol->string asm dst sym)
  (<-code asm "  vm.sp[~a] = vm_symbol_to_string(vm.sp[~a]);\n" dst sym))
(define (emit-char->integer asm dst ch)
  (<-code asm "  vm.sp[~a] = vm_char_to_integer(vm.sp[~a]);\n" dst ch))
(define (emit-integer->char asm dst i)
  (<-code asm "  vm.sp[~a] = vm_integer_to_char(vm.sp[~a]);\n" dst i))
(define (emit-bytevector-length asm dst v)
  (<-code asm "  vm.sp[~a] = vm_bytevector_length(vm.sp[~a]);\n" dst v))
(define (emit-bytevector-u8-ref asm dst v idx)
  (<-code asm "  vm.sp[~a] = vm_bytevector_u8_ref(vm.sp[~a], vm.sp[~a]);\n" dst v idx))
(define (emit-bytevector-u8-set asm v idx val)
  (<-code asm "  vm_bytevector_u8_set(vm.sp[~a], vm.sp[~a], vm.sp[~a]);\n" v idx val))
(define (emit-c-primcall asm prim args)
  (<-code asm "  ~a(~a);\n" prim
          (string-join (map (lambda (arg) (format #f "vm.sp[~a]" arg)) args)
                       ", ")))
(define (emit-c-primcall/result asm prim dst args)
  (<-code asm "  vm.sp[~a] = ~a(~a);\n" dst prim
          (string-join (map (lambda (arg) (format #f "vm.sp[~a]" arg)) args)
                       ", ")))
(define (emit-c-primcall/thread asm prim dst args)
  (<-code asm "  vm.sp[~a] = ~a(vm.thread~{, vm.sp[~a]~});\n" dst prim args))
(define (emit-c-primcall/alloc asm prim dst args junk-slots)
  (<-code asm "  vm.sp[~a] = ~a(vm_trim(vm, ~a)~{, &vm.sp[~a]~});\n" dst prim
          junk-slots args))
(define (emit-jump-if-not-c-primcall asm prim args target)
  (<-code asm "  if (!~a(~a)) goto L~a;\n" prim
          (string-join (map (lambda (arg)
                              (format #f "vm.sp[~a]" arg))
                            args)
                       ", ")
          target))
(define (emit-jump asm target)
  (<-code asm "  goto L~a;\n" target))
(define (emit-jump-if-not-false asm val target)
  (<-code asm "  if (!vm_is_false(vm.sp[~a])) goto L~a;\n" val target))
(define (emit-jump-if-not-fixnum asm val target)
  (<-code asm "  if (!vm_is_fixnum(vm.sp[~a])) goto L~a;\n" val target))
(define (emit-jump-if-not-pair asm val target)
  (<-code asm "  if (!vm_is_pair(vm.sp[~a])) goto L~a;\n" val target))
(define (emit-jump-if-not-vector asm val target)
  (<-code asm "  if (!vm_is_vector(vm.sp[~a])) goto L~a;\n" val target))
(define (emit-jump-if-not-string asm val target)
  (<-code asm "  if (!vm_is_string(vm.sp[~a])) goto L~a;\n" val target))
(define (emit-jump-if-not-symbol asm val target)
  (<-code asm "  if (!vm_is_symbol(vm.sp[~a])) goto L~a;\n" val target))
(define (emit-jump-if-not-char asm val target)
  (<-code asm "  if (!vm_is_char(vm.sp[~a])) goto L~a;\n" val target))
(define (emit-jump-if-not-bytevector asm val target)
  (<-code asm "  if (!vm_is_bytevector(vm.sp[~a])) goto L~a;\n" val target))
(define (emit-jump-if-not-box asm val target)
  (<-code asm "  if (!vm_is_box(vm.sp[~a])) goto L~a;\n" val target))
(define (emit-jump-if-not-eq asm a b target)
  (<-code asm "  if (!vm_is_eq(vm.sp[~a], vm.sp[~a])) goto L~a;\n" a b target))
(define (emit-jump-if-not-< asm a b target)
  (<-code asm "  if (!vm_is_less(vm.sp[~a], vm.sp[~a])) goto L~a;\n" a b target))
(define (emit-jump-if-not-= asm a b target)
  (<-code asm "  if (!vm_is_numerically_eq(vm.sp[~a], vm.sp[~a])) goto L~a;\n" a b target))

(define-syntax-rule (define-record-type/keywords rtd
                      make-rtd pred (field getter init) ...)
  (begin
    (define-record-type rtd (%make-rtd field ...) pred (field getter) ...)
    (define* (make-rtd #:key (field init) ...)
      (%make-rtd field ...))))

(define-record-type/keywords <primitive>
  make-primitive
  primitive?
  (name primitive-name (error "name required"))
  (nargs primitive-nargs (error "nargs required"))
  (has-result? primitive-has-result? #f)
  (predicate? primitive-predicate? #f)
  (emit primitive-emitter (error "emitter required"))
  (allocates? primitive-allocates? #f))

(define *primitives* (make-hash-table))
(define (lookup-primitive name) (hashq-ref *primitives* name))

(define-syntax-rule (define-primitive primitive kw ...)
  (hashq-set! *primitives* 'primitive
              (make-primitive #:name 'primitive kw ...)))

(define-syntax-rule (define-primitives (primitive kw ...) ...)
  (begin (define-primitive primitive kw ...) ...))

(define-primitives
  (exact-integer?   #:nargs 1 #:predicate? #t  #:emit emit-jump-if-not-fixnum)
  (+                #:nargs 2 #:has-result? #t #:emit emit-add)
  (-                #:nargs 2 #:has-result? #t #:emit emit-sub)
  (*                #:nargs 2 #:has-result? #t #:emit emit-mul)
  (quotient         #:nargs 2 #:has-result? #t #:emit emit-quo)
  (remainder        #:nargs 2 #:has-result? #t #:emit emit-rem)

  (pair?            #:nargs 1 #:predicate? #t  #:emit emit-jump-if-not-pair)
  (cons             #:nargs 2 #:has-result? #t #:emit emit-cons
                    #:allocates? #t)
  (car              #:nargs 1 #:has-result? #t #:emit emit-car)
  (cdr              #:nargs 1 #:has-result? #t #:emit emit-cdr)
  (set-car!         #:nargs 2                  #:emit emit-set-car)
  (set-cdr!         #:nargs 2                  #:emit emit-set-cdr)
  
  (allocate-vector  #:nargs 1 #:has-result? #t #:emit emit-allocate-vector
                    #:allocates? #t)
  (make-vector      #:nargs 2 #:has-result? #t #:emit emit-make-vector
                    #:allocates? #t)
  (vector?          #:nargs 1 #:predicate? #t  #:emit emit-jump-if-not-vector)
  (vector-length    #:nargs 1 #:has-result? #t #:emit emit-vector-length)
  (vector-ref       #:nargs 2 #:has-result? #t #:emit emit-vector-ref)
  (vector-set!      #:nargs 3                  #:emit emit-vector-set)
  (vector-init!     #:nargs 3                  #:emit emit-vector-init)
  
  (string?          #:nargs 1 #:predicate? #t  #:emit emit-jump-if-not-string)
  (string->vector   #:nargs 1 #:has-result? #t #:emit emit-string->vector)

  (symbol?          #:nargs 1 #:predicate? #t  #:emit emit-jump-if-not-symbol)
  (symbol->string   #:nargs 1 #:has-result? #t #:emit emit-symbol->string)

  (char?            #:nargs 1 #:predicate? #t  #:emit emit-jump-if-not-char)
  (char->integer    #:nargs 1 #:has-result? #t #:emit emit-char->integer)
  (integer->char    #:nargs 1 #:has-result? #t #:emit emit-integer->char)

  (bytevector?      #:nargs 1 #:predicate? #t  #:emit emit-jump-if-not-bytevector)
  (bytevector-length #:nargs 1 #:has-result? #t #:emit emit-bytevector-length)
  (bytevector-u8-ref #:nargs 2 #:has-result? #t #:emit emit-bytevector-u8-ref)
  (bytevector-u8-set! #:nargs 3                #:emit emit-bytevector-u8-set)
  
  (variable?        #:nargs 1 #:predicate? #t  #:emit emit-jump-if-not-box)
  (make-variable    #:nargs 1 #:has-result? #t #:emit emit-box
                    #:allocates? #t)
  (variable-ref     #:nargs 1 #:has-result? #t #:emit emit-box-ref)
  (variable-set!    #:nargs 2                  #:emit emit-box-set)

  (false?           #:nargs 1 #:predicate? #t  #:emit emit-jump-if-not-false)
  (eq?              #:nargs 2 #:predicate? #t  #:emit emit-jump-if-not-eq)
  (<                #:nargs 2 #:predicate? #t  #:emit emit-jump-if-not-<)
  (=                #:nargs 2 #:predicate? #t  #:emit emit-jump-if-not-=)

  ;; The primitives below aren't recognized from source code; instead
  ;; they are introduced by the canonicalizer.
  (return           #:nargs 1                  #:emit emit-return))

(define (evaluate-args-eagerly src inits k)
  (match inits
    (() (k '()))
    ((init . inits)
     (with-lexicals src (init)
       (evaluate-args-eagerly src inits
                              (lambda (inits)
                                (k (cons init inits))))))))

(define (canonicalize exp)
  (define (visit exp ctx)
    (define (for-value exp) (visit exp 'value))
    (define (for-effect exp) (visit exp 'effect))
    (define (for-tail exp) (visit exp ctx))

    (define (drop exp) (make-primcall #f 'drop (list exp)))
    (define (wrap exp) (make-seq #f exp (make-const #f *unspecified*)))
    (define (return exp) (make-primcall #f 'return (list exp)))

    (match exp
      (($ <void> src)
       (if (eq? ctx 'effect)
           exp
           (for-tail (make-const #f *unspecified*))))

      ((or ($ <const>) ($ <lexical-ref>))
       (match ctx
         ('value exp)
         ('effect (make-void #f))
         ('tail (return exp))))

      (($ <call> src proc args)
       (let ((proc (for-value proc))
             (args (map for-value args)))
         (define exp (make-call src proc args))
         (match ctx
           ('effect (drop exp))
           ('value exp)
           ('tail exp))))

      (($ <let> src names syms vals body)
       (let ((vals (map for-value vals))
             (body (for-tail body)))
         (make-let src names syms vals body)))

      (($ <fix> src names syms vals body)
       (let ((vals (map for-value vals))
             (body (for-tail body)))
         (make-fix src names syms vals body)))

      (($ <lexical-set> src name sym val)
       (let ((val (for-value val)))
         (define exp (make-lexical-set src name sym val))
         (match ctx
           ('effect exp)
           ('value (wrap exp))
           ('tail (return (wrap exp))))))

      (($ <seq> src head tail)
       (let ((head (for-effect head))
             (tail (for-tail tail)))
         (match head
           (($ <void>) tail)
           (_ (make-seq src head tail)))))

      ;; Ensure the test of a conditional is a branching primcall.
      (($ <conditional> src test consequent alternate)
       (let ((test (for-value test))
             (consequent (for-tail consequent))
             (alternate (for-tail alternate)))
         (define (true? x) (match x (($ <const> _ val) val) (_ #f)))
         (define (false? x) (match x (($ <const> _ val) (not val)) (_ #f)))
         (match test
           (($ <conditional> _ test (? true?) (? false?))
            (make-conditional src test consequent alternate))
           (($ <conditional> _ test (? false?) (? true?))
            (make-conditional src test alternate consequent))
           (_
            (make-conditional src (make-primcall src 'false? (list test))
                              alternate consequent)))))

      ((or ($ <module-ref>) ($ <toplevel-ref>)
           ($ <module-set>) ($ <toplevel-set>)
           ($ <toplevel-define>))
       (error "module-level vars not supported" exp))

      (($ <letrec>)
       (error "fix-letrec should have removed letrec"))

      (($ <let-values>)
       (error "let-values not supported"))

      (($ <primitive-ref> src name)
       (error "primitives as values not supported in this compiler."))

      ;; Lambdas have to have only required args and no alternate.
      (($ <lambda> src meta clause)
       (match clause
         (($ <lambda-case> src req #f #f #f () vars body #f)
          (let ((exp (make-lambda src meta
                                  (make-lambda-case src req #f #f #f '() vars
                                                    (visit body 'tail)
                                                    #f))))
            (match ctx
              ('value exp)
              ('effect (make-void #f))
              ('tail (return exp)))))
         (_ (error "case-lambda / lambda* unsupported" exp))))

      ;; No prompts.
      ((or ($ <abort>) ($ <prompt>))
       (error "prompts not supported"))

      ;; Remaining cases are all primcalls.
      (($ <primcall> src name args)
       (match (cons name args)
         (('null? x) (for-tail (make-primcall src 'eq?
                                              (list x (make-const src '())))))
         (('>= a b) (for-tail (make-primcall src '<= (list b a))))
         (('>  a b) (for-tail (make-primcall src '<  (list b a))))
         ;; As long as we have no NaN, we can turn <= into <.
         (('<= a b) (for-tail
                     (make-conditional src
                                       (make-primcall src '< (list b a))
                                       (make-const src #f)
                                       (make-const src #t))))

         (('list . contents)
          (for-tail
           (evaluate-args-eagerly
            src args
            (lambda (args)
              (fold-right (lambda (arg tail)
                            (make-primcall src 'cons (list arg tail)))
                          (make-const #f '())
                          args)))))

         (('vector . contents)
          (for-tail
           (evaluate-args-eagerly
            src args
            (lambda (args)
              (let* ((len (length args))
                     (v (make-primcall src 'allocate-vector
                                       (list (make-const #f len)))))
                (with-lexicals src (v)
                  (fold-right
                   (match-lambda*
                     (((idx . arg) tail)
                      (make-seq src
                                (make-primcall src 'vector-init!
                                               (list v (make-const #f idx) arg))
                                tail)))
                   v
                   (map cons (iota len) args))))))))

         (((or 'call-c-primitive
               'call-c-primitive/result
               'call-c-primitive/thread
               'call-c-primitive/alloc
               'call-c-primitive/pred)
           ($ <const> _ (? string? c-prim))
           . args)
          (let ((exp (make-primcall src `(,name ,c-prim) (map for-value args))))
            (match name
              ((or 'call-c-primitive/result
                   'call-c-primitive/thread
                   'call-c-primitive/alloc)
               (match ctx
                 ('value exp)
                 ('effect (drop exp))
                 ('tail (return exp))))
              ('call-c-primitive
               (match ctx
                 ('value (wrap exp))
                 ('effect exp)
                 ('tail (return (wrap exp)))))
              ('call-c-primitive/pred
               (make-conditional src exp
                                 (for-tail (make-const src #t))
                                 (for-tail (make-const src #f)))))))

         ;; Now that we handled special cases, ensure remaining primcalls
         ;; are understood by the code generator, and if not, error.
         (_
          (let ((prim (lookup-primitive name))
                (args (map for-value args)))
            (unless (and prim
                         (or (not (primitive-nargs prim))
                             (= (primitive-nargs prim) (length args))))
              (error "unsupported primcall" exp))
            (let ((exp (make-primcall src name args)))
              (cond
               ((primitive-predicate? prim)
                (make-conditional src exp
                                  (for-tail (make-const src #t))
                                  (for-tail (make-const src #f))))
               ((primitive-has-result? prim)
                (match ctx
                  ('value exp)
                  ('effect (drop exp))
                  ('tail (return exp))))
               (else
                (match ctx
                  ('value (wrap exp))
                  ('effect exp)
                  ('tail (return (wrap exp)))))))))))))

  (visit exp 'value))

(define-record-type <closure>
  (make-closure label code free-vars)
  closure?
  (label closure-label)
  (code closure-code)
  (free-vars closure-free-vars))

;; Identify closures and assigned variables within X.
(define (split-closures exp)
  (define closures '())
  (define assigned (make-hash-table))
  (define next-closure-label 0)
  (define (make-label!)
    (let ((label next-closure-label))
      (set! next-closure-label (1+ label))
      label))

  ;; Compute free variables in X, adding entries to `free-vars' as
  ;; lambdas are seen, and adding set! vars to `assigned'.
  (define (visit-closure exp)
    (define (visit exp)
      (define (adjoin sym f) (lset-adjoin eq? f sym))
      (define (union f1 f2) (lset-union eq? f1 f2))
      (define (union3 f1 f2 f3) (union f1 (union f2 f3)))
      (define (difference f1 f2) (lset-difference eq? f1 f2))
      (define (visit* xs) (fold (lambda (x free) (union (visit x) free))
                                '() xs))

      (match exp
        (($ <lexical-ref> src name sym)
         (list sym))

        ((or ($ <const>) ($ <void>))
         '())

        (($ <lambda> src meta body)
         (let ((free (visit-closure body))
               (label (make-label!)))
           (set! closures
                 (cons (make-closure label exp free)
                       closures))
           free))

        (($ <lambda-case> src req #f #f #f () gensyms body #f)
         (difference (visit body) gensyms))

        (($ <call> src proc args)
         (union (visit proc) (visit* args)))

        (($ <primcall> src name args)
         (visit* args))

        (($ <conditional> src test consequent alternate)
         (union3 (visit test) (visit consequent) (visit alternate)))

        (($ <lexical-set> src name gensym exp)
         (hashq-set! assigned gensym #t)
         (adjoin gensym (visit exp)))

        (($ <seq> src head tail)
         (union (visit head) (visit tail)))

        (($ <let> src names syms vals body)
         (union (visit* vals)
                (difference (visit body) syms)))

        (($ <fix> src names gensyms funs body)
         (difference (union (visit* funs) (visit body))
                     gensyms))))

    (visit exp))

  (match exp
    (($ <lambda>)
     (match (visit-closure exp)
       (() (values closures assigned))
       (vars
        (error "unexpected free vars" vars))))
    (_
     (error "expected root expr to be a lambda"))))

(define (compute-frame-size clause)
  "Compute how many stack slots will be needed in the frame with for the
lambda-case clause @var{clause}."
  (define (visit-args xs)
    (let lp ((count 0) (size 0) (xs xs))
      (match xs
        (() size)
        ((x . xs)
         (lp (1+ count)
             (max size (+ count (visit x)))
             xs)))))

  (define (visit exp)
    (match exp
      (($ <void>)
       0)

      ((or ($ <const>) ($ <lexical-ref>) ($ <lambda>))
       1)

      (($ <call> src proc args)
       (visit-args (cons proc args)))

      (($ <primcall> src 'drop (x))
       (visit x))

      (($ <primcall> src ((or 'call-c-primitive
                              'call-c-primitive/pred) c-prim) args)
       (visit-args args))
      (($ <primcall> src ((or 'call-c-primitive/result
                              'call-c-primitive/thread
                              'call-c-primitive/alloc) c-prim) args)
       (max 1 (visit-args args)))

      (($ <primcall> src name args)
       (let* ((prim (lookup-primitive name))
              (has-result? (primitive-has-result? prim)))
         (max (visit-args args) (if has-result? 1 0))))

      (($ <conditional> src test consequent alternate)
       (max (visit test)
            (visit consequent)
            (visit alternate)))

      (($ <lexical-set> src name gensym exp)
       (visit exp))

      (($ <seq> src head tail)
       (max (visit head) (visit tail)))

      (($ <let> src names syms vals body)
       (+ (visit-args vals) (visit body)))

      (($ <fix> src names syms vals body)
       (+ (visit-args vals) (visit body)))))

  (match clause
    (($ <lambda-case> src req #f #f #f () syms body #f)
     (+ 1                     ; One slot for the closure.
        (length syms)         ; One slot for each arg.
        (visit body)))))      ; Max lexical/temp count.

(define (compile-closure asm closure assigned? lookup-closure)
  (define (compile-body clause free-vars frame-size)
    (define (sp-offset frame-idx) (- frame-size frame-idx))
    (define (env-sp-offset env) (sp-offset (length env)))
    (define return-offset (sp-offset 1))

    (define (lookup-bound sym env)
      (match (memq sym env)
        (#f #f)
        (env (env-sp-offset env))))
    (define (lookup-free sym) (list-index free-vars sym))
    (define (push-local sym env) (cons sym env))
    (define (initial-env) (push-local 'closure '()))
    (define (push-temp env) (cons #f env))
    (define (stack-size env) (length env))

    (define (init-closure closure vars env)
      (for-each
       (lambda (sym i)
         (cond
          ((lookup-bound sym env)
           => (lambda (idx)
                (emit-closure-init/bound asm closure i idx)))
          ((lookup-free sym)
           => (lambda (idx)
                (define self-idx (lookup-bound 'closure env))
                (emit-closure-init/free asm closure i self-idx idx)))
          (else (error "unbound lexical"))))
       vars (iota (length vars))))
            
    (define (push-args vals env)
      (match vals
        (() env)
        ((val . vals)
         (push-args vals (for-push val env)))))

    (define (push-let syms vals env)
      (match (vector syms vals)
        (#(() ()) env)
        (#((sym . syms) (val . vals))
         (let ((env (begin
                      (for-push val env)
                      (push-local sym env))))
           (when (assigned? sym)
             (let ((dst (env-sp-offset env)))
               (emit-box asm dst dst dst)))
           (push-let syms vals env)))))

    (define (push-fix syms vals env)
      (let ((env (fold
                  (lambda (sym val env)
                    (match (lookup-closure val)
                      (($ <closure> label code free-vars)
                       (let ((junk-slots (env-sp-offset env))
                             (env (push-local sym env)))
                         (emit-make-closure asm (env-sp-offset env)
                                            label (length free-vars)
                                            junk-slots)
                         env))))
                  env syms vals)))
        (for-each
         (lambda (sym val)
           (match (lookup-closure val)
             (($ <closure> label code free-vars)
              (init-closure (lookup-bound sym env) free-vars env))))
         syms vals)
        env))

    (define (for-test exp kf env)
      (match exp
        (($ <primcall> src ('call-c-primitive/pred c-prim) args)
         (let* ((env (push-args args env))
                (arg-offsets (reverse (iota (length args) (env-sp-offset env)))))
           (emit-jump-if-not-c-primcall asm c-prim arg-offsets kf)))

        (($ <primcall> src name args)
         (let ((prim (lookup-primitive name)))
           (unless (primitive-predicate? prim)
             (error "unexpected" exp))
           (let ((env (push-args args env))
                 (emit (primitive-emitter prim)))
             (match (primitive-nargs prim)
               (1 (emit asm (env-sp-offset env) kf))
               (2 (emit asm (env-sp-offset (cdr env)) (env-sp-offset env)
                        kf))))))))

    (define (for-value-at exp env dst)
      (define (maybe-mov dst*)
        (unless (eqv? dst dst*)
          (emit-mov asm dst dst*)))
      (match exp
        (($ <const> src val)
         (let ((env (push-temp env)))
           (emit-constant-ref asm dst val)))

        (($ <lexical-ref> src name sym)
         (cond
          ((lookup-bound sym env)
           => (lambda (idx)
                (if (assigned? sym)
                    (emit-box-ref asm dst idx)
                    (maybe-mov idx))))
          ((lookup-free sym)
           => (lambda (idx)
                (define closure-idx (lookup-bound 'closure env))
                (emit-free-ref asm dst closure-idx idx)
                (when (assigned? sym)
                  (emit-box-ref asm dst dst))))
          (else (error "unbound lexical"))))

        (($ <lambda> src meta clause)
         (let* ((junk-slots (env-sp-offset env))
                (env (push-temp env))
                (dst (env-sp-offset env)))
           (match (lookup-closure exp)
             (($ <closure> label code free-vars)
              (emit-make-closure asm dst label (length free-vars) junk-slots)
              (init-closure dst free-vars env)
              (maybe-mov dst)))))

        (($ <call> src proc args)
         (let* ((base (env-sp-offset env))
                (pre-call (push-args (cons proc args) env))
                (post-call (push-temp env))
                (dst (env-sp-offset post-call)))
           (emit-call asm base (1+ (length args)))
           (emit-restore-sp asm dst)
           (maybe-mov dst)))

        (($ <primcall> src ('call-c-primitive/result c-prim) args)
         (let* ((env (push-args args env))
                (arg-offsets (reverse
                              (iota (length args) (env-sp-offset env)))))
           (emit-c-primcall/result asm c-prim dst arg-offsets)
           (maybe-mov dst)))

        (($ <primcall> src ('call-c-primitive/thread c-prim) args)
         (let* ((env (push-args args env))
                (arg-offsets (reverse
                              (iota (length args) (env-sp-offset env)))))
           (emit-c-primcall/thread asm c-prim dst arg-offsets)
           (maybe-mov dst)))

        (($ <primcall> src ('call-c-primitive/alloc c-prim) args)
         (let* ((env (push-args args env))
                (args-offset (env-sp-offset env))
                (arg-offsets (reverse
                              (iota (length args) (env-sp-offset env)))))
           (emit-c-primcall/alloc asm c-prim dst arg-offsets
                                  args-offset)
           (maybe-mov dst)))

        (($ <primcall> src 'allocate-vector (($ <const> _ size)))
         (let* ((junk-slots (env-sp-offset env)))
           (emit-allocate-vector asm dst size junk-slots)))

        (($ <primcall> src name args)
         (let ((prim (lookup-primitive name)))
           (unless (primitive-has-result? prim)
             (error "unexpected" exp))
           (let* ((env (push-args args env))
                  (emit (primitive-emitter prim))
                  (args-offset (env-sp-offset env)))
             (match (primitive-nargs prim)
               (1 (let ((op0 args-offset))
                    (if (primitive-allocates? prim)
                        (emit asm dst op0 args-offset)
                        (emit asm dst op0))))
               (2 (let ((op0 (1+ args-offset))
                        (op1 args-offset))
                    (if (primitive-allocates? prim)
                        (emit asm dst op0 op1 args-offset)
                        (emit asm dst op0 op1)))))
             (maybe-mov dst))))

        (($ <conditional> src test consequent alternate)
         (let ((kf (make-label asm))
               (kcont (make-label asm)))
           (for-test test kf env)
           (for-value-at consequent env dst)
           (emit-jump asm kcont)
           (emit-bind-label asm kf)
           (for-value-at alternate env dst)
           (emit-bind-label asm kcont)))

        (($ <seq> src head tail)
         (for-effect head env)
         (for-value-at tail env dst))

        (($ <let> src names syms vals body)
         (for-value-at body (push-let syms vals env) dst))
        (($ <fix> src names syms vals body)
         (for-value-at body (push-fix syms vals env) dst))))

    (define (for-push exp env)
      (let ((env* (push-temp env)))
        (for-value-at exp env (env-sp-offset env*))
        env*))

    (define (for-effect exp env)
      (match exp
        (($ <void>)
         (values))

        (($ <lexical-set> src name sym val)
         (let* ((env (for-push val env))
                (src (env-sp-offset env)))
           (cond
            ((lookup-bound sym env)
             => (lambda (idx)
                  (emit-boxed-bound-set asm idx src)))
            ((lookup-free sym)
             => (lambda (idx)
                  (define closure-idx (lookup-bound 'closure env))
                  (emit-boxed-free-set asm closure-idx idx src)))
            (else (error "unbound lexical"))))
         (values))

        (($ <primcall> src 'drop (arg))
         (for-push arg env)
         (values))

        (($ <primcall> src ('call-c-primitive c-prim) args)
         (let* ((env (push-args args env))
                (arg-offsets (reverse
                              (iota (length args) (env-sp-offset env)))))
           (emit-c-primcall asm c-prim arg-offsets)
           (values)))

        (($ <primcall> src 'vector-init!
            (($ <lexical-ref> _ _ v) ($ <const> _ idx) val))
         (let* ((env (for-push val env))
                (val (env-sp-offset env)))
           (match (lookup-bound v env)
             (#f (error "impossorous"))
             (v (emit-vector-init asm v idx val)))))

        (($ <primcall> src name args)
         (let ((prim (lookup-primitive name)))
           (when (primitive-has-result? prim)
             (error "unexpected" exp))
           (when (primitive-allocates? prim)
             (error "unexpected" exp))
           (let ((env (push-args args env))
                 (emit (primitive-emitter prim)))
             (match (primitive-nargs prim)
               (1 (emit asm
                        (env-sp-offset env)))
               (2 (emit asm
                        (env-sp-offset (cdr env))
                        (env-sp-offset env)))
               (3 (emit asm
                        (env-sp-offset (cddr env))
                        (env-sp-offset (cdr env))
                        (env-sp-offset env))))))
         (values))

        (($ <conditional> src test consequent alternate)
         (let ((kf (make-label asm))
               (kcont (make-label asm)))
           (for-test test kf env)
           (for-effect consequent env)
           (emit-jump asm kcont)
           (emit-bind-label asm kf)
           (for-effect alternate env)
           (emit-bind-label asm kcont))
         (values))

        (($ <seq> src head tail)
         (for-effect head env)
         (for-effect tail env))

        (($ <let> src names syms vals body)
         (for-effect body (push-let syms vals env)))
        (($ <fix> src names syms vals body)
         (for-effect body (push-fix syms vals env)))))

    (define (for-tail exp env)
      (match exp
        (($ <primcall> src 'return (val))
         (for-value-at val env return-offset)
         (emit-return asm (1- frame-size)))

        (($ <call> src proc args)
         (let ((base (env-sp-offset env))
               (nargs (1+ (length args))))
           (push-args (cons proc args) env)
           (emit-tail-call asm frame-size base nargs)))

        (($ <conditional> src test consequent alternate)
         (let ((kf (make-label asm)))
           (for-test test kf env)
           (for-tail consequent env)
           (emit-bind-label asm kf)
           (for-tail alternate env)))

        (($ <seq> src head tail)
         (for-effect head env)
         (for-tail tail env))

        (($ <let> src names syms vals body)
         (for-tail body (push-let syms vals env)))
        (($ <fix> src names syms vals body)
         (for-tail body (push-fix syms vals env)))))

    (match clause
      (($ <lambda-case> src req #f #f #f () syms body #f)
       (let* ((env (fold push-local (initial-env) syms))
              (nargs (length env)))
         (emit-assert-nargs-= asm nargs)
         (for-each (lambda (sym idx)
                     (when (assigned? sym)
                       (pk 'boxing! sym)
                       (emit-box asm idx idx 0)))
                   syms (reverse (iota (length syms))))
         (emit-expand-sp asm (- frame-size nargs))
         (for-tail body env)))))

  (match closure
    (($ <closure> label ($ <lambda> src meta clause) free)
     (emit-begin-function asm label)
     (compile-body clause free (compute-frame-size clause))
     (emit-end-function asm))))

(define (call-with-assembler proc)
  (define asm (make-asm (open-output-string)
                        (open-output-string)
                        (make-hash-table)
                        0 0))
  (proc asm)
  (string-append (get-output-string (asm-decl-port asm))
                 "\n"
                 (get-output-string (asm-code-port asm))))

(define* (compile-to-c exp #:key (optimization-level 2)
                       (warning-level 2))
  (define optimize-tree-il (make-lowerer optimization-level
                                         '(#:resolve-primitives? #f)))
  (call-with-values (lambda ()
                      (split-closures
                       (canonicalize
                        (optimize-tree-il
                         (let ((src (tree-il-src exp)))
                           (make-lambda
                            src '()
                            (make-lambda-case src '() #f #f #f '() '() exp #f)))
                         #f))))
    (lambda (closures assigned)
      (call-with-assembler
       (lambda (asm)
         (<-decl asm "#include \"whiffle/vm.h\"\n\n")
         (let ((by-code (make-hash-table)))
           (for-each (lambda (closure)
                       (hashq-set! by-code (closure-code closure) closure))
                     closures)
           (define (assigned? sym)
             (hashq-ref assigned sym))
           (define (lookup-closure x)
             (or (hashq-ref by-code x) (error "missing <closure>" x)))
           (for-each (lambda (closure)
                       (compile-closure asm closure assigned? lookup-closure))
                     (reverse closures)))
         (match closures
           ((($ <closure>
                label
                ($ <lambda> src meta
                   ($ <lambda-case> src () #f #f #f () () body #f))
                free) . _)
            ;; Ensure main is interned as a static closure.
            (constant-ref asm (make-static-closure label))
            (<-code asm "int main (int argc, char *argv[]) {\n")
            (<-code asm "  struct vm_process process;\n")
            (<-code asm "  Thread thread;\n")
            (<-code asm "  VM vm = vm_prepare_process(&process, &thread, &argc, &argv);\n")
            (<-code asm "  vm = vm_expand_stack(vm, 1);\n")
            (emit-constant-ref asm "0" (make-static-closure label))
            (<-code asm "  vm = F~a(vm, 1);\n" label)
            (<-code asm "  if (argc) {\n")
            (<-code asm "    vm = vm_expand_stack(vm, argc);\n")
            (<-code asm "    for (int i = 0; i < argc; i++)\n")
            (<-code asm "      vm.sp[argc-1-i] = vm_parse_value(vm_trim(vm, argc-i), argv[i]);\n")
            (<-code asm "    vm = vm_closure_code(vm.sp[argc])(vm, argc+1);\n")
            (<-code asm "  }\n")
            (<-code asm "  return vm_finish_process(&thread, &process);\n")
            (<-code asm "};\n"))))))))
