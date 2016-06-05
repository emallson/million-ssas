(library (on2 infmax ctvm traversal (1))
  (export dfs bfs)
  (import (scheme)
          (on2 infmax ctvm graph)
          (only (srfi :1 lists) lset-union)
          (only (srfi :26) cut)
          (pfds queues)
          (pfds sets))

  (define (any< a b)
    "< dispatching on the type of a"
    (cond
     [(symbol? a) (string<? (symbol->string a) (symbol->string b))]
     [(string? a) (string<? a b)]
     [(number? a) (< a b)]))

  (define (bfs g v type explore?)
    (do ([queue (enqueue (make-queue) v) (let-values ([(head remaining) (dequeue queue)])
                                           (fold-left enqueue
                                                      remaining
                                                      (filter (lambda (n) (not (set-member? seen n)))
                                                              (filter-edges g head type explore?))))]
         [seen (set-insert (make-set any<) v) (if (queue-empty? queue)
                                                  seen
                                                  (let-values ([(head _) (dequeue queue)])
                                                    (set-insert seen head)))])
        ((queue-empty? queue) seen)))

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
