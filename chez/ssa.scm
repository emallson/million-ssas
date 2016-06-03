(library (on2 infmax ctvm graph (1))
  (export make-graph graph-nodes graph-in-edges graph-out-edges graph-costs graph-benefits read-graph)
  (import (rnrs)
          (rnrs hashtables))

  (define-record-type graph
    (fields
     (immutable nodes)
     (immutable in-edges)
     (immutable out-edges)
     (immutable costs)
     (immutable benefits)))

  (define (read-graph-size port)
    "Reads the number of nodes and edges in the graph from the port."
    (values (read port) (read port)))

  (define (read-ctvm-node port)
    "Reads a node from the port with associated cost and benefit."
    (list (read port) (read port) (read port)))

  (define (read-ctvm-edge port)
    "Reads an edge from the port with associated weight."
    (list (read port) (read port) (read port)))

  (define (read-graph port)
    "Reads a CTVM graph from the given port."
    (let*-values (((num-nodes num-edges) (read-graph-size port))
                  ((node-specs) (do ((node-list '() (cons (read-ctvm-node port) node-list))
                                      (i 0 (+ i 1)))
                                     ((= i num-nodes) node-list)))
                  ((edge-specs) (do ((edge-list '() (cons  (read-ctvm-edge port) edge-list))
                                     (i 0 (+ i 1)))
                                    ((= i num-edges) edge-list))))
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
