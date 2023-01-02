#lang racket/base

(require file/lz4/frame
         racket/runtime-path
         rackunit)

(define-runtime-path examples "examples")

(define (read-frame in)
  (define out (open-output-bytes))
  (read-frame! in out #:validate-content-checksum? #t)
  (get-output-bytes out))

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
    #"The Project Gutenberg eBook of The King James Bible")
   (test-case "block-dependency"
     (let ([bs (read-frame/file (build-path examples "king-james.bd.lz4"))])
       (check-equal?
        (subbytes bs 0 51)
        #"The Project Gutenberg eBook of The King James Bible")
       (check-equal?
        (subbytes bs (- (bytes-length bs) 60))
        #"subscribe to our email newsletter to hear about new eBooks.\n")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests frame-suite))
