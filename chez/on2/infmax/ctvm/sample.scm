(library (on2 infmax ctvm sample (1))
  (export explore-ic explore-lt
          reverse-influence-sample
          weighted-sample
          weighted-ris)
  (import (scheme)
          (only (srfi :1 lists) zip list-tabulate)
          (srfi :16)
          (only (srfi :43 vectors) vector-fold)
          (only (on2 infmax ctvm graph) in-edges graph-benefits)
          (on2 infmax ctvm traversal))

  (define (explore-ic node weight)
    "Given an edge weight, decide whether to explore it under the IC
model."
    (<= (random 1.0) weight))

  (define (explore-lt node weight)
    (error 'explore-lt "Not yet implemented"))

  (define (reverse-influence-sample graph v model)
    "Produce a single RI sample rooted at v on the graph under the
given model."
    (bfs graph v in-edges (case model
                            [(lt) explore-lt]
                            [(ic) explore-ic])))

  (define (weighted-sample values weights)
    "Weighted random sampling.

Re-implemented from Hung's code. Do not know what the original
algorithm is called."
    (let ([r (random 1.0)]
          [n (vector-length values)])
      (do ([left 0 (if (fl>= r (vector-ref weights center))
                       (fx1+ center)
                       left)]
           [right (fx- n 1) (if (fl< r (vector-ref weights (fx- center 1)))
                                (fx1- center)
                                right)]
           [center (fx/ n 2) (fx/ (fx+ right left) 2)]
           [i 0 (fx1+ i)])
          [(or
            (fx>= i n)
            (and
             (fl>= r (vector-ref weights (fx- center 1)))
             (fl< r (vector-ref weights center))))
           center])))

  (define (weighted-ris graph model n)
    (let-values (((nodes benefits) (hashtable-entries (graph-benefits graph))))
      (list-tabulate n (lambda (_)
                         (reverse-influence-sample
                          graph (weighted-sample nodes benefits) model))))))
