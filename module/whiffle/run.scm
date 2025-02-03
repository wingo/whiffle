;;; Lightweight Scheme compiler directly to C.
;;; Copyright (C) 2023, 2025 Andy Wingo.

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

(define-module (whiffle run)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (whiffle compile)
  #:use-module (whiffle features)
  #:use-module (whiffle input)
  #:use-module (whiffle paths)
  #:export (run))

(define (gc-options-alist->str options)
  (string-join (map (match-lambda
                      ((k . v) (format #f "~a=~a" k v)))
                    options)
               ","))

(define (call-with-timeout proc on-timeout secs)
  (define lock (make-mutex))
  (lock-mutex lock)
  (define cvar (make-condition-variable))
  (define cancelled? #f)
  (define (now)
    (match (gettimeofday)
      ((s . us)
       (+ s (* us 1e-6)))))
  (define deadline (+ (now) secs))
  (define thread
    (call-with-new-thread
     (lambda ()
       (lock-mutex lock)
       (let lp ()
         (cond
          (cancelled?
           (unlock-mutex lock))
          ((< deadline (now))
           (set! cancelled? #t)
           (unlock-mutex lock)
           (on-timeout))
          (else
           (wait-condition-variable cvar lock deadline)
           (lp)))))))
  (unlock-mutex lock)
  (define (cancel!)
    (lock-mutex lock)
    (unless cancelled?
      (set! cancelled? #t)
      (signal-condition-variable cvar))
    (unlock-mutex lock))
  (define-values results (proc))
  (cancel!)
  (join-thread thread)
  (apply values results))

(define (call-with-timeout* proc on-timeout timeout)
  (if timeout
      (call-with-timeout proc on-timeout timeout)
      (proc)))

(define* (spawn-and-read-output prog+args success failure
                                #:key echo-output echo-error timeout)
  (match (pipe)
    ((input . output)
     (let ((pid (spawn (car prog+args) prog+args
                       #:output output
                       #:error (or echo-error output))))
       (close-port output)
       (define-values (accum get-bytes) (open-bytevector-output-port))
       (call-with-timeout*
        (lambda ()
          (let lp ()
            (match (get-bytevector-some input)
              ((? eof-object?)
               (close-port input))
              (frag
               (put-bytevector accum frag)
               (when echo-output (put-bytevector echo-output frag))
               (lp)))))
        (lambda ()
          (kill pid SIGTERM))
        timeout)
       (define str (utf8->string (get-bytes)))
       (match (waitpid pid)
         ((_ . 0)
          (success str))
         ((_ . status)
          (failure str status)))))))

(define (precise-gc? gc)
  (and (not (string-contains gc "bdw"))
       (not (string-contains gc "conservative"))))

(define* (run #:key input expr output-file assemble? (args '())
              preserve-builddir?
              (optimization-level 2) (warning-level 2)
              (gc "semi")
              ;; Default heap size: 10 MB.
              (heap-size #e10e6)
              (heap-size-policy 'fixed)
              (print-stats? #f)
              (parallelism (current-processor-count))
              (echo-output? #f)
              (echo-error? #f)
              (fail (lambda (format-string . args)
                      (apply format (current-error-port) format-string args)
                      (exit 1)))
              check-heap-consistency?
              (timeout 300))
  (when (and (or output-file assemble?) (pair? args))
    (fail "unexpected args while only compiling or assembling"))
  (define c-code
    (compile-to-c (parameterize
                      ((check-heap-consistency-feature check-heap-consistency?)
                       (precise-gc-feature (precise-gc? gc)))
                    (if expr
                        (expand expr)
                        (read-and-expand input)))
                  #:optimization-level optimization-level
                  #:warning-level warning-level))

  (cond
   ((and assemble? (not preserve-builddir?))
    (let ((port (if output-file
                    (open-output-file output-file)
                    (current-output-port))))
      (put-string port c-code)
      (force-output port)))
   (else
    (let* ((dir (mkdtemp (temp-filename "whiffle-XXXXXX"))))
      (call-with-output-file (in-vicinity dir "out.c")
        (lambda (port)
          (put-string port c-code)))
      (define build.mk (whiffle-build.mk))
      (call-with-output-file (in-vicinity dir "Makefile")
        (lambda (port)
          (format port "include ~a\n" build.mk)))
      (unless assemble?
        (let ((status (system* "make" "--no-print-directory" "-C" dir "V=0"
                               (string-append "GC_COLLECTOR=" gc)
                               (format #f "-j~a" (current-processor-count))
                               "out")))
          (unless (zero? (status:exit-val status))
            (fail (string-append
                   "error: failed to compile generated C; leaving temp dir ~a\n"
                   "error: try again via `make -C ~a out\n")
                  dir dir))))
      (define gc-options
        `((heap-size . ,heap-size)
          (heap-size-policy
           . ,(case heap-size-policy
                ((fixed) 0)
                ((growable) 1)
                ((adaptive) 2)
                (else (error "bad policy" heap-size-policy))))
          (parallelism . ,parallelism)))
      (define (add-print-stats args)
        (if print-stats? (cons "--print-stats" args) args))
      (define (add-gc-options args)
        (cons* "--gc-options" (gc-options-alist->str gc-options) args))
      (define k
        (cond
         (output-file
          (rename-file (in-vicinity dir "out") output-file)
          (lambda () ""))
         (else
          (spawn-and-read-output
           (cons (in-vicinity dir "out")
                 (add-gc-options (add-print-stats (map object->string args))))
           (lambda (output) (lambda () output))
           (lambda (output status)
             (put-string (current-output-port) output)
             (cond
              ((status:term-sig status)
               => (lambda (sig)
                    (lambda ()
                      (fail "error when running scheme: killed (~a)\n" sig))))
              ((status:stop-sig status)
               => (lambda (sig)
                    (lambda ()
                      (fail "error when running scheme: stopped (~a)\n" sig))))
              ((not (zero? (status:exit-val status)))
               (lambda ()
                 (fail "error when running scheme: failed (~a)\n"
                       (status:exit-val status))))))
           #:echo-output (and echo-output? (current-output-port))
           #:echo-error (and echo-error? (current-error-port))
           #:timeout timeout))))
      (cond
       (preserve-builddir?
        (format #t "preserving builddir; build via `make -C ~a out`\n" dir))
       (else
        (system* "make" "--no-print-directory" "-C" dir "clean" "V=0")
        (delete-file (in-vicinity dir "out.c"))
        (delete-file (in-vicinity dir "Makefile"))
        (rmdir dir)))
      (k)))))
