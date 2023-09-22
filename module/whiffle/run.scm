;;; Lightweight Scheme compiler directly to C.
;;; Copyright (C) 2023 Andy Wingo.

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
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module ((ice-9 threads) #:select (current-processor-count))
  #:use-module (whiffle compile)
  #:use-module (whiffle features)
  #:use-module (whiffle input)
  #:use-module (whiffle paths)
  #:export (run))

(define (bytevector-concatenate args)
  (call-with-output-bytevector
   (lambda (p)
     (for-each (lambda (bv) (put-bytevector p bv)) args))))

(define (gc-options-alist->env-str options)
  (string-append
   "GC_OPTIONS="
   (string-join (map (match-lambda
                       ((k . v) (format #f "~a=~a" k v)))
                     options)
                ",")))

(define* (spawn-and-read-output prog+args success failure
                                #:key echo-port gc-options)
  (match (pipe)
    ((input . output)
     (let ((pid (spawn (car prog+args) prog+args
                       #:environment
                       ;; FIXME: Pass by --gc-option instead.
                       (if gc-options
                           (list (gc-options-alist->env-str gc-options))
                           '())
                       #:output output
                       #:error output)))
       (close-port output)
       (define str
         (utf8->string
          (bytevector-concatenate
           (let lp ()
             (match (get-bytevector-some input)
               ((? eof-object?) '())
               (frag
                (when echo-port (put-bytevector echo-port frag))
                (cons frag (lp))))))))
       (close-port input)
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
              (parallelism (current-processor-count))
              (echo-output? #f)
              (fail (lambda (format-string . args)
                      (apply format (current-error-port) format-string args)
                      (exit 1)))
              check-heap-consistency?)
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
      (define k
        (cond
         (output-file
          (rename-file (in-vicinity dir "out") output-file)
          (lambda () ""))
         (else
          (spawn-and-read-output
           (cons (in-vicinity dir "out") (map object->string args))
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
           #:gc-options `((heap-size . ,heap-size)
                          (heap-size-policy
                           . ,(case heap-size-policy
                                ((fixed) 0)
                                ((growable) 1)
                                ((adaptive) 2)
                                (else (error "bad policy" heap-size-policy))))
                          (parallelism . ,parallelism))
           #:echo-port (and echo-output? (current-output-port))))))
      (cond
       (preserve-builddir?
        (format #t "preserving builddir; build via `make -C ~a out`\n" dir))
       (else
        (system* "make" "--no-print-directory" "-C" dir "clean" "V=0")
        (delete-file (in-vicinity dir "out.c"))
        (delete-file (in-vicinity dir "Makefile"))
        (rmdir dir)))
      (k)))))
