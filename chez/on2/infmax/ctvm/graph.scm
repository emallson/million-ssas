(library (on2 infmax ctvm graph (1))
  (export graph? make-graph nodes out-edges in-edges cost graph-costs benefit graph-benefits
          map-edges filter-edges read-graph)
  (import (scheme)
          (only (srfi :1 lists) zip)
          (rnrs hashtables))

  (define-record-type graph
    (fields
     (immutable nodes nodes)
     (immutable in-edges)
     (immutable out-edges)
     (immutable costs graph-costs)
     (immutable benefits graph-benefits)))

  (define (out-edges g v)
    "Looks up the out-edges of v on graph g."
    (hashtable-ref (graph-out-edges g) v (make-eqv-hashtable)))

  (define (in-edges g v)
    "Looks up the in-edges of v on graph g."
    (hashtable-ref (graph-in-edges g) v (make-eqv-hashtable)))

  (define (map-edges g v type f)
    "Maps function f over the edges produced by (type g v)."
    (let-values (((next-nodes edge-weights) (hashtable-entries (type g v))))
      (map f
           (vector->list next-nodes)
           (vector->list edge-weights))))

  (define (filter-edges g v type f)
    "Filters function f over the edges produced by (type g v)."
    (let-values (((next-nodes edge-weights) (hashtable-entries (type g v))))
      (map car (filter (lambda (pair)
                         (f (car pair) (cadr pair)))
                       (zip (vector->list next-nodes)
                            (vector->list edge-weights))))))

  (define (cost g v)
    "Looks up the cost of v on graph g."
    (hashtable-ref (graph-costs g) v #f))

  (define (benefit g v)
    "Looks up the benefit of v on graph g."
    (hashtable-ref (graph-benefits g) v #f))

  (define (read-graph-size port)
    "Reads the number of nodes and edges in the graph from the port."
    (let* ((num-nodes (read port))
           (num-edges (read port)))
      (values num-nodes num-edges)))

  (define (read-ctvm-node port)
    "Reads a node from the port with associated cost and benefit."
    (let* ((node (read port))
           (cost (read port))
           (benefit (read port)))
      (list node cost benefit)))

  (define (read-ctvm-edge port)
    "Reads an edge from the port with associated weight."
    (let* ((from (read port))
           (to (read port))
           (weight (read port)))
      (list from to weight)))

  (define (read-graph port)
    "Reads a CTVM graph from the given port."
    (let*-values (((num-nodes num-edges) (read-graph-size port))
                  ((node-specs)
                   (map (lambda (_) (read-ctvm-node port)) (iota num-nodes)))
                  ((edge-specs)
                   (map (lambda (_) (read-ctvm-edge port)) (iota num-edges))))
      ;; using mutability simply because hashtables only have mutable
      ;; functions. w/e
      (let ((nodes (map (lambda (ns) (let-values (((node _1 _2) (apply values ns))) node)) node-specs))
            (in-edges (make-eqv-hashtable))
            (out-edges (make-eqv-hashtable))
            (costs (make-eqv-hashtable))
            (benefits (make-eqv-hashtable)))
        (for-each
         (lambda (node-spec)
           (let-values (((node cost benefit) (apply values node-spec)))
             (hashtable-set! costs node cost)
             (hashtable-set! benefits node benefit)))
         node-specs)
        (for-each
         (lambda (edge-spec)
           (let-values (((from to weight) (apply values edge-spec)))
             (hashtable-update! out-edges from
                                (lambda (edge-table)
                                  (hashtable-set! edge-table to weight)
                                  edge-table)
                                (make-eqv-hashtable))
             (hashtable-update! in-edges to
                                (lambda (edge-table)
                                  (hashtable-set! edge-table from weight)
                                  edge-table)
                                (make-eqv-hashtable))))
         edge-specs)
        (make-graph nodes in-edges out-edges costs benefits)))))
