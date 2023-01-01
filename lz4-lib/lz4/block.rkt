#lang racket/base

;; https://github.com/lz4/lz4/blob/8a31e6402df11c1bf8fbb1db3b29ec2c76fe6f26/doc/lz4_Block_format.md

(#%declare #:unsafe)

(require racket/fixnum
         racket/unsafe/ops
         "buffer.rkt")

(provide
 read-block!)

(define (read-block! buf bs)
  (define bs-len
    (unsafe-bytes-length bs))
  (let loop ([src-pos 0]
             [dst-pos (buffer-pos buf)])
    (define token (unsafe-bytes-ref bs src-pos))
    (define-values (literals-len literals-pos)
      (read-length (fxrshift token 4) bs (fx+ src-pos 1)))
    (define offset-pos (fx+ literals-pos literals-len))
    (when (fx> offset-pos literals-pos)
      (buffer-write! buf bs literals-pos offset-pos))
    (when (< offset-pos bs-len)
      (define offset
        (fxior (unsafe-bytes-ref bs offset-pos)
               (fxlshift (unsafe-bytes-ref bs (fx+ offset-pos 1)) 8)))
      (when (fx= offset 0)
        (error 'read-block "corrupted block: zero offset"))
      (define-values (match-len next-src-pos)
        (read-length (fxand token #x0F) bs (fx+ offset-pos 2)))
      (let match-loop ([dst-pos (fx+ dst-pos literals-len)]
                       [match-len (fx+ match-len 4)])
        (define lo (fx- dst-pos offset))
        (define hi (fx+ lo match-len))
        (when (fx< lo 0)
          (error 'read-block "invalid offset"))
        (cond
          [(fx> hi dst-pos)
           (define len (fx- dst-pos lo))
           (define match-len* (fx- match-len len))
           (buffer-copy! buf buf lo dst-pos)
           (if (fx= match-len* 0)
               (loop next-src-pos (fx+ dst-pos len))
               (match-loop (fx+ dst-pos len) match-len*))]
          [else
           (buffer-copy! buf buf lo hi)
           (loop next-src-pos (fx+ dst-pos match-len))])))))

(define (read-length len bs pos)
  (if (fx< len 15)
      (values len pos)
      (let loop ([n len]
                 [p pos])
        (define b (unsafe-bytes-ref bs p))
        ((if (fx< b 255) values loop)
         (fx+ n b)
         (fx+ p 1)))))
