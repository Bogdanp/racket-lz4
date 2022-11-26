#lang racket/base

(require file/lz4/block
         (submod file/lz4/block private)
         rackunit)

(define (read-length* bs)
  (define-values (len _)
    (read-token (open-input-bytes bs)))
  len)

(define (read-sequence* bs)
  (read-sequence (open-input-bytes bs)))

(define block-tests
  (test-suite
   "block"

   (test-suite
    "token"

    (check-equal? (read-length* (bytes #x10)) 1)
    (check-equal? (read-length* (bytes #xE0)) 14)
    (check-equal? (read-length* (bytes #xF0 #x00)) 15)
    (check-equal? (read-length* (bytes #xF0 #x21)) 48)
    (check-equal? (read-length* (bytes #xF0 #xFF #x0A)) 280))

   (test-suite
    "sequence"

    (check-equal?
     (read-sequence* (bytes #x00))
     (sequence #"" #f #f))
    (check-equal?
     (read-sequence* (bytes #x50 65 66 67 68 69))
     (sequence #"ABCDE" #f #f)))

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
