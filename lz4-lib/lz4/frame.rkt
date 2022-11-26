#lang racket/base

;; https://github.com/lz4/lz4/blob/8a31e6402df11c1bf8fbb1db3b29ec2c76fe6f26/doc/lz4_Frame_format.md

(require racket/port
         "block.rkt"
         "buffer.rkt"
         "common.rkt")

(provide
 read-frame!)

(define max-preload-block-size
  (* 10 1024 1024))

(define end-mark
  (bytes #x00 #x00 #x00 #x00))

(define (read-frame! buf in)
  (define magic-number
    (read-integer 'frame "magic number" in))
  (cond
    [(= magic-number #x184D2204)
     (define-values (_block-independence? block-checksum? _content-size content-checksum?)
       (read-frame-descriptor in))
     (let loop ()
       (define block-size-bs
         (expect-bytes 'frame "block size" 4 in))
       (unless (bytes=? block-size-bs end-mark)
         (define block-size+flag
           (integer-bytes->integer block-size-bs #f #f))
         (define compressed?
           (zero? (bitwise-and block-size+flag #x80000000)))
         (define block-size
           (bitwise-and block-size+flag #x7FFFFFFF))
         (define limited-in
           (if (< block-size max-preload-block-size)
               (open-input-bytes (read-bytes block-size in))
               (make-limited-input-port in block-size #f)))
         (if compressed?
             (read-block! buf limited-in)
             (buffer-write! buf (expect-bytes 'read-frame "literal block" block-size limited-in)))
         (when block-checksum?
           (void (expect-bytes 'read-frame "block checksum" 4 in)))
         (loop)))
     (when content-checksum?
       (void (expect-bytes 'read-frame "content checksum" 4 in)))]
    [(and (>= magic-number #x184D2A50)
          (<= magic-number #x184D2A5F))
     (define frame-size
       (read-integer 'frame "skippable frame size" in))
     (void (expect-bytes 'read-frame "skippable frame data" frame-size in))]
    [else
     (error 'read-frame "invalid magic number")]))

(define (read-frame-descriptor in)
  (define flag (expect-byte 'read-frame-descriptor "flag" in))
  (unless (= (arithmetic-shift flag -6) 1)
    (error 'read-frame-descriptor "unsupported version"))
  (void (expect-byte 'read-frame-descriptor "block descriptor" in))
  (define block-independence?
    (on? flag #b00100000))
  (define block-checksum?
    (on? flag #b00010000))
  (define content-size?
    (on? flag #b00001000))
  (define content-checksum?
    (on? flag #b00000100))
  (when (on? flag #b00000010)
    (error 'read-frame-descriptor "reserved bit 1 must be off"))
  (when (on? flag #b00000001)
    (error 'read-frame-descriptor "dictionaries are not supported"))
  (define content-size
    (and content-size?
         (integer-bytes->integer
          (expect-bytes 'read-frame-descriptor "content size" 8 in)
          #f #f)))
  (void (expect-byte 'read-frame-descriptor "header checksum" in))
  (values
   block-independence?
   block-checksum?
   content-size
   content-checksum?))

(define (read-integer who what in)
  (integer-bytes->integer (expect-bytes who what 4 in) #f #f))

(define (on? bits mask)
  (= (bitwise-and bits mask) mask))
