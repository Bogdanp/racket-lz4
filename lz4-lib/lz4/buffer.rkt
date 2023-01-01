#lang racket/base

(#%declare #:unsafe)

(require racket/fixnum
         racket/match
         racket/unsafe/ops)

(provide
 buffer?
 make-buffer
 buffer-str
 buffer-pos
 buffer-reset!
 buffer-write!
 buffer-copy!
 get-buffer-bytes
 copy-buffer)

(struct buffer
  ([str #:mutable]
   [pos #:mutable]))

(define (make-buffer [cap 65535])
  (buffer (make-bytes cap) 0))

(define (get-buffer-bytes b)
  (match-define (buffer buf pos) b)
  (subbytes buf 0 pos))

(define (buffer-reset! b)
  (set-buffer-pos! b 0))

(define (buffer-write! b bs [lo 0] [hi (unsafe-bytes-length bs)])
  (match-define (buffer buf pos) b)
  (define cap (unsafe-bytes-length buf))
  (define len (fx- hi lo))
  (cond
    [(fx> len (fx- cap pos))
     (increase-cap! b len)
     (buffer-write! b bs lo hi)]
    [else
     (unsafe-bytes-copy! buf pos bs lo hi)
     (set-buffer-pos! b (fx+ pos len))]))

(define (buffer-copy! dst src [lo 0] [hi #f])
  (match-define (buffer src-buf src-pos) src)
  (buffer-write! dst src-buf lo (or hi src-pos)))

(define (increase-cap! b amt)
  (match-define (buffer src pos) b)
  (define dst
    (make-bytes
     (fx+ (unsafe-bytes-length src)
          (fxmax amt 65535))))
  (unsafe-bytes-copy! dst 0 src 0 pos)
  (set-buffer-str! b dst))

(define (copy-buffer dst b)
  (match-define (buffer buf pos) b)
  (write-bytes buf dst 0 pos))
