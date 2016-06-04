(library (on2 infmax ctvm traversal (1))
  (export dfs bfs)
  (import (rnrs)
          (on2 infmax ctvm graph)
          (only (srfi :1 lists) lset-union)
          (only (srfi :26) cut)
          (srfi :117 list-queues))

  (define (bfs g v type explore?)
    (do ([queue (list-queue v) queue]
         [current v (unless (list-queue-empty? queue)
                      (list-queue-front queue))]
         [seen (list v) (if (list-queue-empty? queue)
                            seen (cons (list-queue-front queue) seen))])
        ((list-queue-empty? queue) seen)
      (for-each (cut list-queue-add-back! queue <>)
                (filter
                 (lambda (node) (not (member node seen)))
                 (filter-edges g current type explore?)))
      (list-queue-remove-front! queue)))

  (define dfs
    (case-lambda
      [(g v type explore?) (dfs g v type explore? '())]
      [(g v type explore? seen)
       (if (member v seen)
           ;; already seen, return
           '()
           ;; not seen yet, explore from it
           (fold-left (lambda (all-seen u)
                        (let ((children (dfs g u type explore? all-seen)))
                          (lset-union eqv? children all-seen)))
                      (cons v seen)
                      (filter-edges g v type explore?)))])))
