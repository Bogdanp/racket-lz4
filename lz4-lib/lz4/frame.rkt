#lang racket/base

;; https://github.com/lz4/lz4/blob/8a31e6402df11c1bf8fbb1db3b29ec2c76fe6f26/doc/lz4_Frame_format.md

(require "block.rkt"
         "buffer.rkt")

(provide
 read-frame!)

(define end-mark
  (bytes #x00 #x00 #x00 #x00))

(define dependent-block-max-offset
  #xFFFF)

(define (read-frame! in out)
  (define buf (make-buffer (* 4 1024 1024)))
  (define tmp (make-bytes (* 16 1024 1024)))
  (define magic-number
    (read-integer 'frame "magic number" in))
  (cond
    [(= magic-number #x184D2204)
     (define-values (block-independence? block-checksum? _content-size content-checksum?)
       (read-frame-descriptor in))
     (let loop ([lo 0])
       (define block-size-bs
         (expect-bytes 'frame "block size" 4 in))
       (unless (bytes=? block-size-bs end-mark)
         (define block-size+flag
           (integer-bytes->integer block-size-bs #f #f))
         (define compressed?
           (zero? (bitwise-and block-size+flag #x80000000)))
         (define block-size
           (bitwise-and block-size+flag #x7FFFFFFF))
         (define block-bs
           (if (> block-size (bytes-length tmp))
               (make-bytes block-size)
               tmp))
         (expect-bytes! 'read-frame "block" block-bs block-size in)
         (when block-checksum?
           (void (expect-bytes 'read-frame "block checksum" 4 in)))
         (loop
          (cond
            [compressed?
             (read-block! buf block-bs block-size)
             (copy-buffer out buf lo)
             (cond
               [block-independence?
                (begin0 0
                  (buffer-reset! buf))]
               [else
                (define pos
                  (buffer-pos buf))
                (if (> pos dependent-block-max-offset)
                    (begin0 dependent-block-max-offset
                      (buffer-reset-keeping-last! buf dependent-block-max-offset))
                    pos)])]
            [else
             (begin0 lo
               (write-bytes block-bs out 0 block-size))]))))
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

(define (expect-bytes! who what dst amt in)
  (define n-read
    (read-bytes! dst in 0 amt))
  (begin0 n-read
    (unless (equal? n-read amt)
      (error who "unexpected EOF while reading ~a" what))))

(define (expect-byte who what in)
  (define b (read-byte in))
  (begin0 b
    (when (eof-object? b)
      (error who "unexpected EOF while reading ~a" what))))

(define (expect-bytes who what amt in)
  (define bs (read-bytes amt in))
  (begin0 bs
    (when (or (eof-object? bs)
              (< (bytes-length bs) amt))
      (error who "unexpected EOF while reading ~a" what))))
