#lang racket/base

(require racket/contract
         racket/match
         racket/unsafe/ops)

(provide
 (contract-out
  [xxhash32 (->* (bytes?) (u32/c) u32/c)]
  [xxh32? (-> any/c boolean?)]
  [make-xxh32 (->* () (u32/c) xxh32?)]
  [xxh32-reset! (-> xxh32? u32/c void?)]
  [xxh32-update! (->* (xxh32? bytes?)
                      (exact-nonnegative-integer?
                       exact-nonnegative-integer?)
                      void?)]
  [xxh32-digest (-> xxh32? u32/c)]))

(module+ unsafe
  (provide
   make-xxh32
   xxh32-reset!
   xxh32-update!
   xxh32-digest))

(struct xxh32 (len acc1 acc2 acc3 acc4 tmp tmpsize)
  #:mutable)

(define u32/c (integer-in 0 #xFFFFFFFF))
(define u32max #xFFFFFFFF)
(define u32max+1 (add1 u32max))

(define prime1 #x9E3779B1)
(define prime2 #x85EBCA77)
(define prime3 #xC2B2AE3D)
(define prime4 #x27D4EB2F)
(define prime5 #x165667B1)

(define (xxhash32 bs [seed 0])
  (define h (make-xxh32 seed))
  (xxh32-update! h bs)
  (xxh32-digest h))

(define (make-xxh32 [seed 0])
  (define h (xxh32 0 0 0 0 0 #f 0))
  (begin0 h
    (xxh32-reset! h seed)))

(define (xxh32-reset! h seed)
  (set-xxh32-len! h 0)
  (set-xxh32-acc1! h (mod+ (mod+ seed prime1) prime2))
  (set-xxh32-acc2! h (mod+ seed prime2))
  (set-xxh32-acc3! h seed)
  (set-xxh32-acc4! h (mod- seed prime1))
  (set-xxh32-tmp! h (make-bytes 16))
  (set-xxh32-tmpsize! h 0))

(define (xxh32-update! h bs [start 0] [end (unsafe-bytes-length bs)])
  (define bs-len (- end start))
  (when (> bs-len 0)
    (match-define (xxh32 len acc1 acc2 acc3 acc4 tmp tmpsize) h)
    (set-xxh32-len! h (+ len bs-len))
    (cond
      [(< (+ tmpsize bs-len) 16)
       (unsafe-bytes-copy! tmp tmpsize bs start end)
       (set-xxh32-tmpsize! h (+ tmpsize bs-len))]
      [else
       (define lo start)
       (when (> tmpsize 0)
         (define off (+ lo (- 16 tmpsize)))
         (unsafe-bytes-copy! tmp tmpsize bs lo off)
         (set-xxh32-acc1! h (xxh32-round acc1 tmp  0  4))
         (set-xxh32-acc2! h (xxh32-round acc2 tmp  4  8))
         (set-xxh32-acc3! h (xxh32-round acc3 tmp  8 12))
         (set-xxh32-acc4! h (xxh32-round acc4 tmp 12 16))
         (set! lo off)
         (set-xxh32-tmpsize! h 0))
       (define limit
         (- end 16))
       (let loop ([acc1 (xxh32-acc1 h)]
                  [acc2 (xxh32-acc2 h)]
                  [acc3 (xxh32-acc3 h)]
                  [acc4 (xxh32-acc4 h)]
                  [lo lo])
         (cond
           [(<= lo limit)
            (loop (xxh32-round acc1 bs    lo     (+ lo  4))
                  (xxh32-round acc2 bs (+ lo  4) (+ lo  8))
                  (xxh32-round acc3 bs (+ lo  8) (+ lo 12))
                  (xxh32-round acc4 bs (+ lo 12) (+ lo 16))
                  (+ lo 16))]
           [else
            (set-xxh32-acc1! h acc1)
            (set-xxh32-acc2! h acc2)
            (set-xxh32-acc3! h acc3)
            (set-xxh32-acc4! h acc4)
            (when (< lo end)
              (unsafe-bytes-copy! tmp 0 bs lo (+ lo (- end lo)))
              (set-xxh32-tmpsize! h (- end lo)))]))])))

(define (xxh32-digest h)
  (match-define (xxh32 len acc1 acc2 acc3 acc4 tmp tmpsize) h)
  (define h32
    (mod+
     (if (>= len 16)
         (mod+
          (mod+
           (mod+ (modrotl acc1 1)
                 (modrotl acc2 7))
           (modrotl acc3 12))
          (modrotl acc4 18))
         (mod+ acc3 prime5))
     len))
  (xxh32-finalize h32 tmp tmpsize))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (xxh32-round acc bs lo hi)
  (define n (integer-bytes->integer bs #f #f lo hi))
  (mod* (modrotl (mod+ acc (mod* n prime2)) 13) prime1))

(define (xxh32-finalize acc bs len)
  (let-values ([(acc pos) (xxh32-finalize-1 acc bs 0 len)])
    (let* ([acc (xxh32-finalize-2 acc bs pos len)]
           [acc (xor acc (modshr acc 15))]
           [acc (mod* acc prime2)]
           [acc (xor acc (modshr acc 13))]
           [acc (mod* acc prime3)]
           [acc (xor acc (modshr acc 16))])
      acc)))

(define (xxh32-finalize-1 acc bs pos len)
  (let loop ([acc acc]
             [pos pos]
             [len len])
    (cond
      [(>= len 4)
       (define n (integer-bytes->integer bs #f #f pos (+ pos 4)))
       (define acc* (mod* (modrotl (mod+ acc (mod* n prime3)) 17) prime4))
       (loop acc* (+ pos 4) (- len 4))]
      [else
       (values acc pos)])))

(define (xxh32-finalize-2 acc bs pos len)
  (let loop ([acc acc]
             [pos pos]
             [len (- len pos)])
    (cond
      [(< len 1) acc]
      [else
       (define n (bytes-ref bs pos))
       (define acc* (mod* (modrotl (mod+ acc (mod* n prime5)) 11) prime1))
       (loop acc* (add1 pos) (sub1 len))])))


;; math ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mod+ a b)
  (define v (+ a b))
  (if (> v u32max)
      (- v u32max 1)
      v))

(define (mod- a b)
  (define v (- a b))
  (if (< v 0)
      (+ u32max+1 v)
      v))

(define (mod* a b)
  (remainder
   (* (remainder a u32max+1)
      (remainder b u32max+1))
   u32max+1))

(define (modrotl a b)
  (define v (arithmetic-shift a b))
  (bitwise-ior
   (bitwise-and v u32max)
   (bitwise-and (arithmetic-shift v -32) u32max)))

(define (modshr a b)
  (arithmetic-shift (bitwise-and a u32max) (- b)))

(define (xor a b)
  (bitwise-xor a b))
