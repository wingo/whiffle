;;; Lightweight Scheme compiler directly to C.
;;; Copyright (C) 2023, 2024, 2025 Andy Wingo.

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
             (ice-9 pretty-print)
             (ice-9 threads)
             (ice-9 match))

(define (mmc-collectors)
  (define (apply-prefixes prefixes suffixes)
    (append suffixes
            (append-map (lambda (prefix)
                          (map (lambda (suffix)
                                 (symbol-append prefix suffix))
                               suffixes))
                        prefixes)))
  (fold apply-prefixes '(mmc)
        '((generational-)
          (parallel-)
          (stack-conservative- heap-conservative-))))

(define *all-collectors*
  (cons* 'semi 'bdw 'pcc 'generational-pcc (mmc-collectors)))

(define (default-collector-filter collector nthreads parallelism multiplier)
  (cond
   ((eq? collector 'semi)
    (and (eq? nthreads parallelism 1) (>= multiplier 2)))
   ((eq? collector 'bdw) #t)
   ((or (eq? collector 'pcc) (eq? collector 'generational-pcc))
    (>= multiplier 2))
   ((string-contains (symbol->string collector) "parallel") #t)
   (else (eq? parallelism 1))))

(define (last-line s)
  (let* ((s (string-trim-right s)))
    (match (string-index-right s #\newline)
      (#f s)
      (pos (substring s pos)))))

(define* (run-benchmark benchmark args
                        #:key
                        (max-threads (min (current-processor-count) 8))
                        (max-parallelism (min (current-processor-count) 8))
                        (exit-on-failure? #t)
                        (repetitions 1)
                        (heap-size-policy 'fixed)
                        (minimum-serial-heap-size #e10e6)
                        (heap-size-multiplier 1.5)
                        (timeout 30)
                        (initial-heap-size (lambda (nthreads)
                                             (inexact->exact
                                              (floor
                                               (* nthreads
                                                  minimum-serial-heap-size
                                                  heap-size-multiplier)))))
                        (collector-filter default-collector-filter)
                        (echo-output? #f))
  (format #t "running: ~a~{ ~s~}, ~,1fx heap, ~,1f MB per mutator thread:\n" benchmark args
          heap-size-multiplier
          (* minimum-serial-heap-size heap-size-multiplier 1e-6))
  (force-output)
  (let ((filename (whiffle-filename "examples" benchmark)))
    (define (run-configuration collector nthreads parallelism results)
      (match results
        (#(pass fail skip)
         (define configuration
           (list collector #:nthreads nthreads #:parallelism parallelism))
         (cond
          ((collector-filter collector nthreads parallelism heap-size-multiplier)
           (let ((tag (make-prompt-tag)))
             (call-with-prompt
              tag
              (lambda ()
                (format #t " ~a:~:[ ~;\n~]" configuration echo-output?)
                (force-output)
                (define output
                  (run #:input (open-input-file filename)
                       #:args (cons nthreads args)
                       #:gc (symbol->string collector)
                       #:heap-size (initial-heap-size nthreads)
                       #:heap-size-policy heap-size-policy
                       #:parallelism parallelism
                       #:echo-output? echo-output?
                       #:print-stats? #t
                       #:timeout timeout
                       #:fail (lambda (fmt . args)
                                (abort-to-prompt tag fmt args))))
                (define stats (call-with-input-string (last-line output) read))
                (match stats
                  (#(mutator-seconds collector-seconds
                     mutator-cpu-seconds collector-cpu-seconds
                     major minor
                     p50 p95 p100)
                   (format #t "~:[~*~; ~a: ~]pass: ~,3fs (~,3fs gc), ~,3fCPUs (~,3fCPUs gc), ~a major+~a minor, p50 pause ~,3fms (~,3f p95, ~,3f max)\n"
                           echo-output? configuration
                           (+ mutator-seconds collector-seconds)
                           collector-seconds
                           (+ mutator-cpu-seconds collector-cpu-seconds)
                           collector-cpu-seconds
                           major minor (* p50 1e3) (* p95 1e3) (* p100 1e3))))
                (force-output)
                (vector (acons configuration stats pass) fail skip))
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
         (fold (lambda (config results)
                 (match config
                   ((nthreads parallelism)
                    (let lp ((n 0) (results results))
                      (if (< n repetitions)
                          (lp (1+ n)
                              (run-configuration collector nthreads parallelism
                                                 results))
                          results)))))
               results `((1 1)
                         (1 ,max-parallelism)
                         (,max-threads 1)
                         (,max-threads ,max-parallelism))))
       #(() () ())
       *all-collectors*))
    (match (run-configurations)
      (#(() () skip)
       (format #t " all skipped\n"))
      ((and results #(pass fail skip))
       (newline)
       (format #t "passing tests: ~a\n" (length pass))
       (format #t "failing tests: ~a\n" (length fail))
       (format #t "~{  failed test: ~a\n~}" fail)
       (format #t "skipped tests: ~a\n" (length skip))
       (force-output)
       (unless (null? fail)
         (when exit-on-failure?
           (exit 1)))
       results))))

(define* (run-benchmarks #:key (heap-size-multiplier 2.5) (repetitions 1)
                         (exit-on-failure? #t))
  (define results '())
  (define-syntax-rule (run-many ((file . args) . kwargs) ...)
    (begin
      (set! results
            (acons (format #f "~a~{-~a~}-~,1fx" file 'args heap-size-multiplier)
                   (run-benchmark file 'args
                                  #:heap-size-multiplier heap-size-multiplier
                                  #:repetitions repetitions
                                  #:exit-on-failure? exit-on-failure?
                                  . kwargs)
                   results))
      ...
      (reverse results)))

  ;; MMC nominal overhead is 1/16.  PCC is 100%.
  (define (scale-mmc-minimum n)
    (inexact->exact (round (* n 15/16))))
  (define (scale-pcc-minimum n)
    (inexact->exact (round (/ n 2))))

  (run-many
   (("gcbench.scm" 0)         #:minimum-serial-heap-size (scale-mmc-minimum
                                                          #e18.6e6))
   (("gcbench.scm" 1)         #:minimum-serial-heap-size (scale-mmc-minimum
                                                          #e18.6e6))
   (("quads.scm" 10)          #:minimum-serial-heap-size #e65e6)
   (("ephemerons.scm" 500000) #:minimum-serial-heap-size #e40e6)
   (("splay.scm")             #:minimum-serial-heap-size (scale-pcc-minimum
                                                          #e98e6))
   (("cpstak.scm" 32 16 8 9)  #:minimum-serial-heap-size #e1e6)
   (("eval-fib.scm" 32)       #:minimum-serial-heap-size #e1e6)
   (("earley.scm" 14)         #:minimum-serial-heap-size (scale-pcc-minimum
                                                          #e473e6))
   (("peval.scm" 12 1)        #:minimum-serial-heap-size (scale-pcc-minimum
                                                          #e78e6))
   (("nboyer.scm" 4)          #:minimum-serial-heap-size (scale-pcc-minimum
                                                          #e142e6))
   (("nboyer.scm" 5)          #:minimum-serial-heap-size (scale-pcc-minimum
                                                          #e422e6))))

(define (string->count str)
  (let ((c (string->number str)))
    (unless (and c (exact-integer? c) (positive? c))
      (error "expected a positive integer" str))
    c))

(define (string->multiplier str)
  (let ((c (string->number str)))
    (unless (and c (real? c) (>= c 1.0))
      (error "expected a real number greater than 1" str))
    (exact->inexact c)))

(define main
  (match-lambda
    ((_)
     (run-benchmarks)
     (format #t "All tests passed.\n"))
    ((_ out-file)
     (call-with-output-file out-file
       (lambda (port)
         (pretty-print (run-benchmarks #:exit-on-failure? #f) port))))
    ((_ out-file repetitions . heap-size-multipliers)
     (let ((repetitions (string->count repetitions))
           (heap-size-multipliers (map string->multiplier heap-size-multipliers)))
       (call-with-output-file out-file
         (lambda (port)
           (pretty-print
            (append-map (lambda (multiplier)
                          (run-benchmarks #:heap-size-multiplier multiplier
                                          #:repetitions repetitions
                                          #:exit-on-failure? #f))
                        heap-size-multipliers)
            port)))))))

(when (batch-mode?)
  (main (program-arguments))
  (exit 0))
