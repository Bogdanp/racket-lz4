#lang racket/base

;; https://github.com/lz4/lz4/blob/8a31e6402df11c1bf8fbb1db3b29ec2c76fe6f26/doc/lz4_Block_format.md

(require racket/match
         "buffer.rkt"
         "common.rkt")

(provide
 read-block
 read-block!)

(module+ private
  (provide
   (struct-out sequence)
   read-sequence
   read-token))

(define (read-block in)
  (define buf (make-buffer))
  (read-block! buf in)
  (get-buffer-bytes buf))

(define (read-block! buf in)
  (let loop ([pos (buffer-pos buf)])
    (match-define (sequence literals offset matchlen)
      (read-sequence in))
    (define pos* (+ pos (bytes-length literals)))
    (buffer-write! buf literals)
    (when (and offset matchlen)
      (let match-loop ([pos* pos*] [matchlen matchlen])
        (define lo (- pos* offset))
        (define hi (+ lo matchlen))
        (when (negative? lo)
          (error 'read-block "invalid offset"))
        (cond
          [(> hi pos*)
           (define len (- pos* lo))
           (define matchlen* (- matchlen len))
           (buffer-write! buf (buffer-buf buf) lo pos*)
           (if (zero? matchlen*)
               (loop (+ pos* len))
               (match-loop (+ pos* len) matchlen*))]
          [else
           (buffer-write! buf (buffer-buf buf) lo hi)
           (loop (+ pos* matchlen))])))))

(struct sequence (literals offset matchlen)
  #:transparent)

(define (read-sequence in)
  (define-values (len matchlen*)
    (read-token in))
  (define literals
    (expect-bytes 'read-sequence "literals" len in))
  (cond
    [(eof-object? (peek-byte in))
     (sequence literals #f #f)]
    [else
     (define offset
       (read-offset in))
     (define matchlen
       (cond
         [(< matchlen* 15) (+ matchlen* 4)]
         [else (read-length "match length" (+ matchlen* 4) in)]))
     (sequence literals offset matchlen)]))

(define (read-token in)
  (define token
    (expect-byte 'read-token "token" in))
  (define len
    (let ([len (arithmetic-shift token -4)])
      (cond
        [(< len 15) len]
        [else (read-length "sequence length" len in)])))
  (values len (bitwise-and token #x0F)))

(define (read-length what len in)
  (let loop ([len len])
    (define n (expect-byte 'read-length what in))
    (if (= n 255)
        (loop (+ len n))
        (+ len n))))

(define (read-offset in)
  (define offset
    (integer-bytes->integer (expect-bytes 'read-offset "offset" 2 in) #f #f))
  (begin0 offset
    (when (zero? offset)
      (error 'read-offset "zero offset (corrupted block)"))))
