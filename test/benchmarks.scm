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
             ((srfi srfi-1) #:select (fold append-map))
             ((ice-9 threads) #:select (current-processor-count))
             (ice-9 match))

(define (whippet-collectors)
  (define (apply-prefixes prefixes suffixes)
    (append suffixes
            (append-map (lambda (prefix)
                          (map (lambda (suffix)
                                 (symbol-append prefix suffix))
                               suffixes))
                        prefixes)))
  (fold apply-prefixes '(whippet)
        '((generational-)
          (parallel-)
          (stack-conservative- heap-conservative-))))

(define *all-collectors*
  (cons* 'semi 'bdw (whippet-collectors)))

(define (default-collector-filter collector nthreads parallelism)
  (cond
   ((eq? collector 'semi) (eq? nthreads parallelism 1))
   ((eq? collector 'bdw) #t)
   ((string-contains (symbol->string collector) "parallel") #t)
   (else (eq? parallelism 1))))

(define* (run-benchmark benchmark args
                        #:key
                        (max-threads (min (current-processor-count) 8))
                        (max-parallelism (min (current-processor-count) 8))
                        (heap-size-policy 'fixed)
                        (minimum-serial-heap-size #e10e6)
                        (heap-size-multiplier 1.5)
                        (initial-heap-size (lambda (nthreads)
                                             (inexact->exact
                                              (floor
                                               (* nthreads
                                                  minimum-serial-heap-size
                                                  heap-size-multiplier)))))
                        (collector-filter default-collector-filter)
                        (echo-output? #f))
  (format #t "running: ~a~{ ~s~}:\n" benchmark args)
  (force-output)
  (let ((filename (whiffle-filename "examples" benchmark)))
    (define (run-configuration collector nthreads parallelism results)
      (match results
        (#(pass fail skip)
         (define configuration
           (list collector #:nthreads nthreads #:parallelism parallelism))
         (cond
          ((collector-filter collector nthreads parallelism)
           (let ((tag (make-prompt-tag)))
             (call-with-prompt
              tag
              (lambda ()
                (format #t " ~a:~:[ ~;\n~]" configuration echo-output?)
                (force-output)
                (run #:input (open-input-file filename)
                     #:args (cons nthreads args)
                     #:gc (symbol->string collector)
                     #:heap-size (initial-heap-size nthreads)
                     #:heap-size-policy heap-size-policy
                     #:parallelism parallelism
                     #:echo-output? echo-output?
                     #:fail (lambda (fmt . args)
                              (abort-to-prompt tag fmt args)))
                (format #t "~:[~; ~a: ~]pass\n" echo-output? configuration)
                (force-output)
                (vector (cons configuration pass) fail skip))
              (lambda (_ fmt args)
                (format #t "~:[~; ~a: ~]fail\n" echo-output? configuration)
                (force-output)
                (vector pass (cons configuration fail) skip)))))
          (else
           (format #t " ~a: skip\n" configuration)
           (force-output)
           (vector pass fail (cons configuration skip)))))))
    (define (run-configurations)
      (fold
       (lambda (collector results)
         (let lp ((nthreads 1) (results results))
           (if (<= nthreads max-threads)
               (lp (1+ nthreads)
                   (let lp ((parallelism 1) (results results))
                     (if (<= parallelism max-parallelism)
                         (lp (1+ parallelism)
                             (run-configuration collector nthreads parallelism
                                                results))
                         results)))
               results)))
       #(() () ())
       *all-collectors*))
    (match (run-configurations)
      (#(() () skip)
       (format #t " all skipped\n"))
      (#(pass fail skip)
       (newline)
       (format #t "passing tests: ~a\n" (length pass))
       (format #t "failing tests: ~a\n" (length fail))
       (format #t "~{  failed test: ~a\n~}" fail)
       (format #t "skipped tests: ~a\n" (length skip))
       (force-output)
       (unless (null? fail)
         (exit 1))))))

(run-benchmark "gcbench.scm" '()
               #:minimum-serial-heap-size #e20e6
               #:heap-size-multiplier 2.5)
;(check-run "quads.scm" (current-processor-count) 10)
;(check-run "ephemerons.scm" (current-processor-count) 200000)

(format #t "All tests passed.\n")
(exit 0)
