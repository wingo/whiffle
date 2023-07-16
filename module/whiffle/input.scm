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

(define-module (whiffle input)
  #:use-module (ice-9 match)
  #:use-module (language tree-il)
  #:use-module (language tree-il primitives)
  #:export (read-and-expand))

(define (make-fresh-whiffle-module)
  (make-fresh-user-module))

;; How to allow the prelude to define primcalls?  Wrap in a (lambda
;; (primcall) ...), then instances of (primcall 'cons a b) translates to
;; an actual primcall.  Eventually.
(define (expand exp)
  (save-module-excursion
   (lambda ()
     (define mod (make-fresh-whiffle-module))
     (set-current-module mod)
     (resolve-primitives (macroexpand exp) mod))))

(define (read* port)
  (match (read port)
    ((? eof-object?)
     (error "file is empty" file))
    (expr
     `(begin ,expr
             . ,(let lp ()
                  (match (read port)
                    ((? eof-object?) '())
                    (expr (cons expr (lp)))))))))

(define (read-and-expand port)
  (expand (read* port)))

