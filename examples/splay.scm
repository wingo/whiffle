;; Copyright 2023 Andy Wingo
;; Ported to Scheme from Octane's `splay.js', whose copyright is:
;;
;; Copyright 2009 the V8 project authors. All rights reserved.
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials provided
;;       with the distribution.
;;     * Neither the name of Google Inc. nor the names of its
;;       contributors may be used to endorse or promote products derived
;;       from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; This benchmark is based on a JavaScript log processing module used
;; by the V8 profiler to generate execution time profiles for runs of
;; JavaScript applications, and it effectively measures how fast the
;; JavaScript engine is at allocating nodes and reclaiming the memory
;; used for old nodes. Because of the way splay trees work, the engine
;; also has to deal with a lot of changes to the large tree object
;; graph.

;; Configuration.
(define tree-size 8000)
(define tree-modifications 80)
(define tree-payload-depth 5)

(define (make-leaf array key)
  (vector 'leaf array (vector "String for key " key " in leaf mode")))
(define (make-branch left right)
  (vector 'branch left right))

(define (make-payload-tree depth tag)
  (if (zero? depth)
      (make-leaf (vector 0 1 2 3 4 5 6 7 8 9) tag)
      (make-branch (make-payload-tree (1- depth) tag)
                   (make-payload-tree (1- depth) tag))))

;; Constructs a splay tree.  A splay tree is a self-balancing binary
;; search tree with the additional property that recently accessed
;; elements are quick to access again.  It performs basic operations
;; such as insertion, look-up and removal in O(log(n)) amortized time.
(define (make-tree) (box #f))
(define (tree-root tree) (box-ref tree))
(define (set-tree-root! tree node) (box-set! tree node))
(define (tree-empty? tree) (not (tree-root tree)))

(define (make-node k v) (vector k v #f #f))
(define (node-key node) (vector-ref node 0))
(define (node-value node) (vector-ref node 1))
(define (node-left node) (vector-ref node 2))
(define (node-right node) (vector-ref node 3))
(define (set-node-left! node left) (vector-set! node 2 left))
(define (set-node-right! node right) (vector-set! node 3 right))

;; Inserts a node into the tree with the specified key and value if the
;; tree does not already contain a node with the specified key.  If the
;; value is inserted, it becomes the root of the tree.
(define (tree-insert! tree key value)
  (cond
   ((tree-empty? tree)
    (set-tree-root! tree (make-node key value)))
   (else
    ;; Splay on the key to move the last node on the search path for the
    ;; key to the root of the tree.
    (let* ((root (tree-splay! tree key))
           (root-key (node-key root)))
      (unless (eq? root-key key)
        (let ((node (make-node key value)))
          (cond
           ((< root-key key)
            (set-node-left! node root)
            (set-node-right! node (node-right root))
            (set-node-right! root #f))
           (else
            (set-node-right! node root)
            (set-node-left! node (node-left root))
            (set-node-left! root #f)))
          (set-tree-root! tree node)))))))

;; Removes a node with the specified key from the tree if the tree
;; contains a node with this key.  The removed node is returned.  If the
;; key is not found, an exception is thrown.
(define (tree-remove! tree key)
  (cond
   ((tree-empty? tree)
    (error "key not found" key))
   (else
    ;; Splay on the key to move the last node on the search path for the
    ;; key to the root of the tree.
    (let* ((root (tree-splay! tree key))
           (root-key (node-key root)))
      (unless (eq? (node-key root) key)
        (error "key not found" key))
      (cond
       ((node-left root)
        => (lambda (left)
             (let ((right (node-right root)))
               (set-tree-root! tree left)
               ;; Splay to make sure that the new root has an empty
               ;; right child.
               
               ;; Insert the original right child as the right child of
               ;; the new root.
               (set-node-right! (tree-splay! tree key) right))))
       (else
        (set-tree-root! tree (node-right root))))
      root))))

;; Returns the node having the specified key or #f if the tree doesn't
;; contain a node with the specified key.
(define (tree-find tree key)
  (and (not (tree-empty? tree))
       (let ((root (tree-splay! tree key)))
         (and (eq? (node-key root) key)
              root))))

(define (node-find-max start)
  (let lp ((cur start))
    (cond
     ((node-right cur) => lp)
     (else cur))))
(define (tree-find-max tree)
  (node-find-max (tree-root tree)))

(define (tree-find-greatest-less-than tree key)
  (define (and=> x f) (and x (f x)))
  (and (not (tree-empty? tree))
       ;; Splay on the key to move the node with the given key or the
       ;; last node on the search path to the top of the tree.
       (let ((root (tree-splay! tree key)))
         (if (< (node-key root) key)
             root
             (and=> (node-left root) node-find-max)))))

(define (tree-keys tree)
  (let lp ((node (tree-root tree)) (keys '()))
    (if node
        (let ((left (node-left node))
              (keys (cons (node-key node)
                          (lp (node-right node) keys))))
          (if left
              (lp left keys)
              keys))
        keys)))

;; Perform the splay operation for the given key.  Moves the node with
;; the given key to the top of the tree.  If no node has the given key,
;; the last node on the search path is moved to the top of the tree.
;; This is the simplified top-down splaying algorithm from:
;; "Self-adjusting Binary Search Trees" by Sleator and Tarjan
(define (tree-splay! tree key)
  (cond
   ((tree-empty? tree) #f)
   (else
    ;; Create a dummy node.  The use of the dummy node is a bit
    ;; counter-intuitive: The right child of the dummy node will hold
    ;; the L tree of the algorithm.  The left child of the dummy node
    ;; will hold the R tree of the algorithm.  Using a dummy node, left
    ;; and right will always be nodes and we avoid special cases.
    (let ((dummy (make-node #f #f)))
      (define (reassemble current left right)
        (set-node-right! left (node-left current))
        (set-node-left! right (node-right current))
        (set-node-left! current (node-right dummy))
        (set-node-right! current (node-left dummy))
        (set-tree-root! tree current)
        current)
      (let lp ((current (tree-root tree))
               (left dummy)
               (right dummy))
        (let ((cur-key (node-key current)))
          (cond
           ((< key cur-key)
            (let ((cur-left (node-left current)))
              (cond
               ((not cur-left) (reassemble current left right))
               ((< key (node-key cur-left))
                ;; Rotate right.
                (set-node-left! current (node-right cur-left))
                (set-node-right! cur-left current)
                (cond
                 ((node-left cur-left)
                  (set-node-left! right cur-left)
                  (lp (node-left cur-left) left cur-left))
                 (else (reassemble cur-left left right))))
               (else
                ;; Link right.
                (set-node-left! right current)
                (lp cur-left left current)))))
           ((> key cur-key)
            (let ((cur-right (node-right current)))
              (cond
               ((not cur-right) (reassemble current left right))
               ((> key (node-key cur-right))
                ;; Rotate left.
                (set-node-right! current (node-left cur-right))
                (set-node-left! cur-right current)
                (cond
                 ((node-right cur-right)
                  ;; Link left.
                  (set-node-right! left cur-right)
                  (lp (node-right cur-right) cur-right right))
                 (else (reassemble cur-right left right))))
               (else
                (set-node-right! left current)
                (lp cur-right current right)))))
           (else
            (reassemble current left right)))))))))

(define (run-test thread-id)
  (define time-samples '())
  (define time-start (current-microseconds))
  (define (update-stats! time)
    (set! time-samples (cons time time-samples))
    (set! time-start time))

  (define tree (make-tree))

  (define (make-key) (random-fixnum!))
  (define (insert-new-node!)
    (let ((key (make-key)))
      (if (tree-find tree key)
          (insert-new-node!)
          (let ((payload (make-payload-tree tree-payload-depth key)))
            (tree-insert! tree key payload)
            key))))

  ;; setup
  (let lp ((i 0))
    (when (< i tree-size)
      (insert-new-node!)
      (when (eq? (%remainder i 20) 18)
        (update-stats! (current-microseconds)))
      (lp (1+ i))))

  ;; Main workload: replace a few nodes in the splay tree.
  (let lp ((i 0))
    (when (< i 200)
      (let lp ((i 0))
        (when (< i tree-modifications)
          (let* ((key (insert-new-node!))
                 (greatest (tree-find-greatest-less-than tree key)))
            (tree-remove! tree
                          (if greatest
                              (node-key greatest)
                              key)))
          (lp (1+ i))))
      (update-stats! (current-microseconds))
      (lp (1+ i))))

  ;; finish: check consistency.
  (let ((keys (tree-keys tree)))
    (unless (eq? (length keys) tree-size)
      (error "splay tree has wrong size"))
    
    (let lp ((keys keys))
      (let ((head (car keys))
            (tail (cdr keys)))
        (when (pair? tail)
          (unless (< head (car tail))
            (error "splay tree not sorted"))
          (lp tail))))))

(lambda (nthreads)
  (parallel nthreads (lambda (i) (run-test i))))

