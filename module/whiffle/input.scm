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
  #:use-module (whiffle paths)
  #:use-module (ice-9 match)
  #:use-module (language tree-il primitives)
  #:export (expand read-and-expand))

(save-module-excursion
 (lambda ()
   (set-current-module (resolve-module '(whiffle primitives)))
   (add-interesting-primitive! 'call-c-primitive)
   (add-interesting-primitive! 'call-c-primitive/result)
   (add-interesting-primitive! 'call-c-primitive/thread)
   (add-interesting-primitive! 'call-c-primitive/alloc)
   (add-interesting-primitive! 'call-c-primitive/pred)))

(define (make-fresh-whiffle-module)
  (let ((mod (make-fresh-user-module)))
    (purify-module! mod)
    (module-use! mod (resolve-interface '(whiffle primitives)))
    mod))

(define (expand exp)
  (let ((exp #`(let ()
                 (include #,(whiffle-prelude.scm))
                 (let ()
                   #,exp))))
    (save-module-excursion
     (lambda ()
       (define mod (make-fresh-whiffle-module))
       (set-current-module mod)
       (resolve-primitives (macroexpand exp) mod)))))

(define (read* port)
  (match (let lp ()
             (match (read-syntax port)
               ((? eof-object?) '())
               (expr (cons expr (lp)))))
    (() (error "file is empty" (port-filename port)))
    (body
     #`(begin . #,body))))

(define (read-and-expand port)
  (expand (read* port)))

