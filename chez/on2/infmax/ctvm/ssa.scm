(library (on2 infmax ctvm ssa)
  (export ssa)
  (import (scheme)
          (on2 infmax ctvm graph)
          (on2 infmax ctvm traversal)
          (on2 infmax ctvm sample)
          (only (srfi :26) cut)
          (pfds sets))

  (define (map-indexed f l)
    "Calls (f v i) where i is the index of item v in l and returns the
resulting list. Order of result not guaranteed (though the i's are
guaranteed to be correct)."
    (do ([rem l (cdr rem)]
         [i 0 (fx1+ i)]
         [mapped '() (if (null? rem)
                         mapped
                         (cons (f i (car rem)) mapped))])
        ((null? rem) mapped)))

  (define (precompute-coverage R v)
    "R is a list of RR sets. v is a node. The result is the set of RR
sets that v covers."
    (list->set (filter (cut set-member? <> v) R) any<))

  (define (precompute-complete-coverage g R)
    "Compute coverage over all nodes and return a hash table.

Because the hash table API is not immutable (or at least because
hashtable-set! has no return value), this function cannot be defined
in a tail-recursive manner."
    (fold-left (lambda (ht node)
                 (hashtable-set! ht node (precompute-coverage R node))
                 ht)
               (make-eqv-hashtable)
               (nodes g)))

  (define (flbinom n k)
    "n choose k, where both n and k are flonums."
    (fold-left (lambda (c ip)
                 (let ([i (fixnum->flonum ip)])
                   (fl* c (/ (- n (- k i)) i))))
               1.0 (iota k)))

  (define (argcmp cmp f l)
    (time (fold-left (lambda (current next)
                       (if (cmp (f next) (f current))
                           next
                           current))
                     (car l) (cdr l))))

  (define (coverage seeds precover)
    (set-size (set-fold (lambda (n cur)
                          ;; (display n)
                          ;; (newline)
                          ;; (display (and (hashtable-ref precover n #f) #t))
                          ;; (newline)
                          (set-union cur (hashtable-ref precover n #f)))
                        (make-set any<)
                         seeds)))

  (define (max-coverage g R k n precover)
    (do (
         [seeds (make-set any<)
                (time (if cur (set-insert
                               seeds
                               cur)
                          seeds))]
         [seeds-coverage 0.0 (coverage seeds precover)]
         [total-cost 0.0 (if (not cur) 0.0 (fl+ total-cost (cost g cur)))]
         [cur #f (time (argcmp > (lambda (node)
                                   ;; (display node)
                                   ;; (newline)
                                   (- (coverage (set-insert seeds node) precover)
                                      seeds-coverage)) (nodes g)))]
         )
        ((> total-cost k) (cons (set-remove seeds cur) (* seeds-coverage (/ n (length R)))))
      (display (set->list seeds))
      (newline)))

  (define (estimate-inf g model S ε2 δ2 Tmax)
    (let ([A2 (fl+ 1.0 (fl* (fl+ 2.0 (fl/ 2.0 3.0) ε2) (fl+ 1.0 ε2) (fllog (fl/ 1.0 δ2)) (fl/ 1.0 (flexpt ε2 2.0))))])
      (do ([i 0 (fx1+ i)]
           [Rj (car (weighted-sample g model 1)) (car (weighted-sample g model 1))]
           [Cov 0 (min (set-size (set-intersection Rj S)) 1)])
          (((or (>= i Tmax)
                (>= Cov A2))
            (if (>= Cov A2)
                (* (length (nodes g)) (/ Cov i))
                -1))))))

  (define (ssa g model k ε δ)
    (let* ([ε1 (fl/ ε 6.0)]
           [ε2 (fl/ ε 2.0)]
           [ε3 (fl/ ε (fl* 4.0 (fl- 1.0 (fl/ 1.0 (flexp 1.0)))))]
           [n (length (nodes g))]
           [A (fl* (fl+ 1.0 ε1)
                   (fl+ 1.0 ε2)
                   (fl+ 2.0 (fl* (fl/ 2.0 3.0) ε3))
                   (fllog (fl/ 3.0 δ))
                   (fl/ 1.0 (flexpt ε3 2.0)))]
           [max-R (fl* (fl+ 8.0 (fl* 2.0 ε))
                       (fixnum->flonum (length (nodes g)))
                       (fl/ (fl+ (fllog (fl/ 2.0 δ))
                                 (fllog (flbinom n k)))
                            (flexpt ε 2.0)))]
           [R1 (weighted-ris g model (flceiling A))]
           [pc1 (precompute-complete-coverage g R1)])
      (do ([R R1 (time (append (weighted-ris g model (length R)) R))]
           [precov pc1 (time (precompute-complete-coverage g R))]
           [seeds+inf (time (max-coverage g R1 k n pc1)) (time (max-coverage g R k n precov))])
          ((or (>= (length R) max-R)
               (and (fl* (cdr seeds+inf) (/ (length R) n))
                    (<= (cdr seeds+inf)
                        (fl* (fl+ 1.0 ε1) (estimate-inf g model (car seeds+inf) ε2 (fl/ δ 3.0) (* (length R)
                                                                                              (fl/ (fl+ 1.0 ε2)
                                                                                                   (fl- 1.0 ε2))
                                                                                              (flexpt (fl/ ε2 ε3) 2.0)))))))
           (car seeds+inf))
        (display (length R))
        (newline))))
  )
