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

(use-modules (whiffle run)
             (whiffle paths)
             (ice-9 match))

(define (check-equal expected actual)
  (unless (equal? expected actual)
    (error (format #f "expected ~s, got ~s" expected actual))))

(define (load-in-fresh-module filename)
  (save-module-excursion
   (lambda ()
     (set-current-module (make-fresh-user-module))
     (primitive-load filename))))

(define (read-values port)
  (let ((datum (read port)))
    (if (eof-object? datum)
        '()
        (cons datum
              (read-values port)))))

(define (parse-output str)
  (call-with-input-string str read-values))

(define (check-run example . args)
  (format #t "checking: ~s~{ ~s~}:" example args)
  (force-output)
  (let ((filename (whiffle-filename "examples" example)))
    (check-equal (parse-output
                  (with-output-to-string
                    (lambda () (apply (load-in-fresh-module filename) args))))
                 (parse-output
                  (run #:input (open-input-file filename)
                       #:args args))))
  (format #t " ok.\n"))

(check-run "peano-fib.scm" 25)

(format #t "All tests passed.\n")
(exit 0)
