(import (scheme)
        (on2 infmax ctvm graph)
        (on2 infmax ctvm sample))
(define grqc (time (call-with-input-file "/home/emallson/Code/cikm16/datasets/snap/processed/ca-GrQc.txt" read-graph)))
(define eager-samples (time (weighted-ris grqc 'ic 1000000)))
