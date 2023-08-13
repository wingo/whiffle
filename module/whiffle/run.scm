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
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (whiffle compile)
  #:use-module (whiffle input)
  #:use-module (whiffle paths)
  #:export (run))

(define* (spawn-and-read-output prog+args success failure)
  (match (pipe)
    ((input . output)
     (let ((pid (spawn (car prog+args) prog+args #:output output)))
       (close-port output)
       (define str (get-string-all input))
       (close-port input)
       (match (waitpid pid)
         ((_ . 0)
          (success str))
         ((_ . status)
          (failure str status)))))))

(define (read-datum-then-eof port)
  (let ((datum (read port)))
    (when (eof-object? datum)
      (error "unexpected EOF"))
    (let ((eof (read port)))
      (unless (eof-object? eof)
        (error "expected EOF" eof)))
    datum))

(define* (run #:key input output-file assemble? (args '())
              (optimization-level 2) (warning-level 2)
              (fail (lambda (format-string . args)
                      (apply format (current-error-port) format-string args)
                      (exit 1))))
  (when (and (or output-file assemble?) (pair? args))
    (fail "unexpected args while only compiling or assembling"))
  (define c-code
    (compile-to-c (read-and-expand input)
                  #:optimization-level optimization-level
                  #:warning-level warning-level))

  (cond
   (assemble?
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
      (let ((status (system* "make" "--no-print-directory" "-C" dir
                             "-f" build.mk "V=0" "out")))
        (unless (zero? (status:exit-val status))
          (fail (string-append
                 "error: failed to compile generated C; leaving temp dir ~a\n"
                 "error: try again via `make -C ~a -f ~a out\n")
                dir dir build.mk)))
      (define k
        (cond
         (output-file
          (rename-file (in-vicinity dir "out") output-file)
          (lambda () (values)))
         (else
          (spawn-and-read-output
           (cons (in-vicinity dir "out") args)
           (lambda (output)
             (lambda ()
               (call-with-input-string output read-datum-then-eof)))
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
                       (status:exit-val status))))))))))
      (system* "make" "--no-print-directory" "-C" dir "-f" build.mk
               "clean" "V=0")
      (delete-file (in-vicinity dir "out.c"))
      (rmdir dir)
      (k)))))
