#lang racket/base

;; https://github.com/lz4/lz4/blob/8a31e6402df11c1bf8fbb1db3b29ec2c76fe6f26/doc/lz4_Block_format.md

(#%declare #:unsafe)

(require racket/fixnum
         racket/match
         "buffer.rkt"
         "common.rkt")

(provide
 read-block!)

(define (read-block! buf in)
  (let loop ([pos (buffer-pos buf)])
    (match-define (sequence literals offset matchlen)
      (read-sequence in))
    (define pos* (fx+ pos (bytes-length literals)))
    (buffer-write! buf literals)
    (when (and offset matchlen)
      (let match-loop ([pos* pos*] [matchlen matchlen])
        (define lo (fx- pos* offset))
        (define hi (fx+ lo matchlen))
        (when (fx< lo 0)
          (error 'read-block "invalid offset"))
        (cond
          [(fx> hi pos*)
           (define len (fx- pos* lo))
           (define matchlen* (fx- matchlen len))
           (buffer-write! buf (buffer-str buf) lo pos*)
           (if (fx= matchlen* 0)
               (loop (fx+ pos* len))
               (match-loop (fx+ pos* len) matchlen*))]
          [else
           (buffer-write! buf (buffer-str buf) lo hi)
           (loop (fx+ pos* matchlen))])))))

(struct sequence (literals offset matchlen)
  #:transparent)

(define (read-sequence in)
  (define token
    (expect-byte 'read-token "token" in))
  (define len
    (read-length "sequence length" (fxrshift token 4) in))
  (define literals
    (expect-bytes 'read-sequence "literals" len in))
  (define offset-bs
    (read-bytes 2 in))
  (cond
    [(eof-object? offset-bs)
     (sequence literals #f #f)]
    [else
     (define offset
       (fx+ (bytes-ref offset-bs 0)
            (fxlshift (bytes-ref offset-bs 1) 8)))
     (when (fx= offset 0)
       (error 'read-sequence "corrupted block: zero offset"))
     (define matchlen
       (fx+ (read-length "match length" (fxand token #x0F) in) 4))
     (sequence literals offset matchlen)]))

(define (read-length what len in)
  (if (fx< len 15) len (read-length* what len in)))

(define (read-length* what len in)
  (let loop ([n len])
    (define b
      (expect-byte 'read-length what in))
    (if (fx< b 255)
        (fx+ n b)
        (loop (fx+ n b)))))
