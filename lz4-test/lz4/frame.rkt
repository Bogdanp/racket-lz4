#lang racket/base

(require file/lz4/frame
         racket/runtime-path
         rackunit)

(define-runtime-path examples "examples")

(define (read-frame/file path)
  (call-with-input-file path read-frame))

(define frame-suite
  (test-suite
   "frame"

   (check-equal?
    (read-frame/file (build-path examples "empty.lz4"))
    #"")
   (check-equal?
    (read-frame/file (build-path examples "short.lz4"))
    #"hello\n")
   (check-equal?
    (read-frame/file (build-path examples "test.lz4"))
    #"test test test test test test\n")
   (check-equal?
    (subbytes (read-frame/file (build-path examples "king-james.lz4")) 0 51)
    #"The Project Gutenberg eBook of The King James Bible")))

(module+ test
  (require rackunit/text-ui)
  (run-tests frame-suite))
