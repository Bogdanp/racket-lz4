#lang racket/base

(require file/lz4/block
         file/lz4/buffer
         rackunit)

(define (read-block in)
  (define buf (make-buffer))
  (read-block! buf in)
  (get-buffer-bytes buf))

(define block-tests
  (test-suite
   "block"

   (test-suite
    "block"

    (check-equal?
     (read-block (open-input-bytes (bytes #x00)))
     #"")
    (check-equal?
     (read-block (open-input-bytes (bytes #x50 65 66 67 68 69)))
     #"ABCDE")
    (check-equal?
     (read-block (open-input-bytes (bytes #x5f ;; token
                                          #x74 #x65 #x73 #x74 #x20 ;; literal
                                          #x05 #x00 ;; offset
                                          #x01 ;; matchlen
                                          #x50 ;; token
                                          #x74 #x65 #x73 #x74 #x0a ;; literal
                                          )))
     #"test test test test test test\n")
    (check-equal?
     (read-block (open-input-bytes (bytes #x1f ;; token
                                          #x61 ;; literal
                                          #x01 #x00 ;; offset
                                          #x05 ;; matchlen
                                          #x50 ;; token
                                          #x61 #x61 #x61 #x61 #x0a ;; literal
                                          )))
     #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n")
    (check-equal?
     (read-block (open-input-bytes (bytes #x3f ;; token
                                          #x61 #x62 #x63 ;; literal
                                          #x03 #x00 ;; offset
                                          #x03 ;; matchlen
                                          #x50 ;; token
                                          #x62 #x63 #x61 #x62 #x0a ;; literal
                                          )))
     #"abcabcabcabcabcabcabcabcabcab\n"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests block-tests))
