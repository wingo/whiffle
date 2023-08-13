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
             (ice-9 match))

(define (check-equal expected actual)
  (unless (equal? expected actual)
    (error (format #f "expected ~s, got ~s" expected actual))))

(define (check-expr expr)
  (format #t "checking: ~s:" expr)
  (force-output)
  (check-equal (primitive-eval expr)
               (run #:input (open-input-string
                             (object->string `(lambda () ',expr)))))
  (format #t " ok.\n"))

(define (check-exprs exprs)
  (for-each check-expr exprs))

(check-exprs
 '(42))

(format #t "All tests passed.\n")
(exit 0)
