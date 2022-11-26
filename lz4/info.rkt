#lang info

(define implies '("lz4-lib"))
(define collection "lz4")
(define deps '("base"
               "lz4-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define scribblings '(("lz4-manual.scrbl")))
