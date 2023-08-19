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

(define-module (whiffle paths)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (ice-9 match)
  #:export (whiffle-filename
            whiffle-build.mk
            whiffle-stdlib.scm
            temp-filename))

(define (resolve filename)
  (cond
   ((search-path %load-path filename)
    => (lambda (filename)
         (dirname (dirname (dirname filename)))))
   (else
    (error "don't know current file name!" filename))))

(define whiffledir
  (delay (resolve (assq-ref (or (current-source-location) '()) 'filename))))

(define tmpdir
  (or (getenv "TMPDIR") "/tmp"))

(define (whiffle-filename . parts)
  (let lp ((result (force whiffledir)) (parts parts))
    (match parts
      (() result)
      ((part . parts) (lp (in-vicinity result part) parts)))))

(define (whiffle-build.mk)
  (whiffle-filename "build.mk"))

(define (whiffle-stdlib.scm)
  (whiffle-filename "runtime" "stdlib.scm"))

(define (temp-filename filename)
  (in-vicinity tmpdir filename))
