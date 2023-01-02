#lang info

(define license 'BSD-3-Clause)
(define implies '("xxhash-lib"))
(define collection "file")
(define deps '("base"
               "xxhash-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define scribblings '(("xxhash-manual.scrbl")))
