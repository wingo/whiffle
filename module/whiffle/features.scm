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

(define-module (whiffle features)
  #:export (whiffle-cond-expand
            check-heap-consistency-feature
            precise-gc-feature))

(define check-heap-consistency-feature (make-parameter #f))
(define precise-gc-feature (make-parameter #f))

(define-syntax whiffle-cond-expand
  (lambda (x)
    (define (condition-matches? condition)
      (syntax-case condition (and or not)
        ((and c ...)
         (and-map condition-matches? #'(c ...)))
        ((or c ...)
         (or-map condition-matches? #'(c ...)))
        ((not c)
         (if (condition-matches? #'c) #f #t))
        (c
         (identifier? #'c)
         (memq (syntax->datum #'c) (whiffle-features)))))

    (define (match clauses alternate)
      (syntax-case clauses ()
        (((condition form ...) . rest)
         (if (condition-matches? #'condition)
             #'(begin form ...)
             (match #'rest alternate)))
        (() (alternate))))

    (syntax-case x (else)
      ((_ clause ... (else form ...))
       (match #'(clause ...)
         (lambda ()
           #'(begin form ...))))
      ((_ clause ...)
       (match #'(clause ...)
         (lambda ()
           (syntax-violation 'cond-expand "unfulfilled cond-expand" x)))))))

(define (whiffle-features)
  (define (add-feature-if test feature features)
    (if test (cons feature features) features))
  (add-feature-if
   (precise-gc-feature) 'precise-gc
   (add-feature-if
    (check-heap-consistency-feature) 'check-heap-consistency
    '())))
