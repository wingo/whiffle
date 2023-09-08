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

(define-module (whiffle primitives)
  #:pure
  #:use-module ((guile)
                #:select
                (include-from-path
                 define-syntax-rule
                 syntax-case syntax quasisyntax unsyntax unsyntax-splicing
                 with-syntax identifier-syntax identifier?
                 lambda* define*

                 cons*
                 eval-when
                 error))
  #:use-module ((srfi srfi-11)
                #:select
                (let-values let*-values))
  #:use-module ((guile)
                #:select
                (_
                 ... => else
                 lambda
                 define define-values let let* letrec letrec*
                 or and
                 begin
                 if cond case when unless
                 do
                 set!
                 quote quasiquote unquote unquote-splicing
                 include
                 define-syntax let-syntax letrec-syntax
                 syntax-rules syntax-error
                 cond-expand

                 pair?
                 cons
                 car
                 cdr
                 set-car!
                 set-cdr!

                 exact-integer?
                 +
                 -
                 *
                 <
                 <=
                 =
                 >
                 >=
                 quotient
                 remainder

                 char->integer
                 integer->char
                 char?

                 eq?

                 string?

                 symbol?
                 symbol->string

                 vector?
                 make-vector
                 vector
                 vector-length
                 vector-ref
                 vector-set!))
  #:re-export
  (_
   ... => else
   lambda
   define define-values let let* letrec letrec* let-values let*-values
   or and
   begin 
   if cond case when unless
   do
   set!
   quote quasiquote unquote unquote-splicing
   include
   define-syntax let-syntax letrec-syntax
   syntax-rules syntax-error
   cond-expand
   include-from-path
   define-syntax-rule
   syntax-case syntax quasisyntax unsyntax unsyntax-splicing
   with-syntax identifier-syntax identifier?
   lambda* define*

   ;; The rest of the primitives can only appear in primcalls, so we
   ;; expose them as %foo instead of foo, relying on the prelude to wrap
   ;; them in lambdas to ensure they are always called with the right
   ;; number of arguments, even when used as a value.

   (pair? . %pair?)
   (cons . %cons)
   (car . %car)
   (cdr . %cdr)
   (set-car! . %set-car!)
   (set-cdr! . %set-cdr!)

   (exact-integer? . %exact-integer?)
   (* . %*)
   (+ . %+)
   (- . %-)
   (< . %<)
   (<= . %<=)
   (= . %=)
   (> . %>)
   (>= . %>=)
   (quotient . %quotient)
   (remainder . %remainder)

   (char? . %char?)
   (char->integer . %char->integer)
   (integer->char . %integer->char)

   (eq? . %eq?)

   (string? . %string?)

   (symbol? . %symbol?)
   (symbol->string . %symbol->string)

   (vector? . %vector?)
   (make-vector . %make-vector)
   (vector . %vector)
   (vector-length . %vector-length)
   (vector-ref . %vector-ref)
   (vector-set! . %vector-set!))
  #:export (call-c-primitive
            call-c-primitive/result
            call-c-primitive/alloc)
  ;; Mark as non-declarative, as we should not have inlinable exports.
  #:declarative? #f)

(define (call-c-primitive prim . args) (error "target-only primitive"))
(define (call-c-primitive/result prim . args) (error "target-only primitive"))
(define (call-c-primitive/alloc prim . args) (error "target-only primitive"))
