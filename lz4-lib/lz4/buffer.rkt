#lang racket/base

(#%declare #:unsafe)

(provide
 buffer?
 make-buffer
 buffer-str
 buffer-pos
 buffer-reset!
 buffer-write!
 get-buffer-bytes
 copy-buffer)

(struct buffer
  ([str #:mutable]
   [pos #:mutable]))

(define (make-buffer [cap 65535])
  (buffer (make-bytes cap) 0))

(define (get-buffer-bytes b)
  (define buf (buffer-str b))
  (define pos (buffer-pos b))
  (subbytes buf 0 pos))

(define (buffer-reset! b)
  (set-buffer-pos! b 0))

(define (buffer-write! b bs [lo 0] [hi (bytes-length bs)])
  (define buf (buffer-str b))
  (define pos (buffer-pos b))
  (define cap (bytes-length buf))
  (define len (- hi lo))
  (cond
    [(> len (- cap pos))
     (increase-cap! b len)
     (buffer-write! b bs lo hi)]
    [else
     (bytes-copy! buf pos bs lo hi)
     (set-buffer-pos! b (+ pos len))]))

(define (increase-cap! b amt)
  (define pos (buffer-pos b))
  (define src (buffer-str b))
  (define dst
    (make-bytes
     (+ (bytes-length src)
        (max amt 65535))))
  (bytes-copy! dst 0 src 0 pos)
  (set-buffer-str! b dst))

(define (copy-buffer dst b)
  (define buf (buffer-str b))
  (define pos (buffer-pos b))
  (write-bytes buf dst 0 pos))
