#lang racket/base

(#%declare #:unsafe)

(require (for-syntax racket/base
                     racket/fixnum
                     racket/require-transform)
         racket/contract
         racket/match
         racket/performance-hint
         racket/require
         racket/unsafe/ops
         (filtered-in
          (λ (name)
            (and (regexp-match #rx"^unsafe-fx" name)
                 (regexp-replace #rx"unsafe-" name "")))
          racket/unsafe/ops))

(define-syntax math-procedures
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ unsafe safe)
        (expand-import
         (if (>= (most-positive-fixnum) #xFFFFFFFFFFFFFFF)
             #'unsafe
             #'safe))]))))

(require
 (math-procedures
  "private/xxhash32/unsafe-math.rkt"
  "private/xxhash32/math.rkt"))

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
  (set-xxh32-acc1! h (uint32+ (uint32+ seed prime1) prime2))
  (set-xxh32-acc2! h (uint32+ seed prime2))
  (set-xxh32-acc3! h seed)
  (set-xxh32-acc4! h (uint32- seed prime1))
  (set-xxh32-tmp! h (make-bytes 16))
  (set-xxh32-tmpsize! h 0))

(define (xxh32-update! h bs [start 0] [end (unsafe-bytes-length bs)])
  (define bs-len (fx- end start))
  (when (fx> bs-len 0)
    (match-define (xxh32 len acc1 acc2 acc3 acc4 tmp tmpsize) h)
    (set-xxh32-len! h (fx+ len bs-len))
    (cond
      [(fx< (fx+ tmpsize bs-len) 16)
       (unsafe-bytes-copy! tmp tmpsize bs start end)
       (set-xxh32-tmpsize! h (fx+ tmpsize bs-len))]
      [else
       (define lo start)
       (when (fx> tmpsize 0)
         (define off (fx+ lo (fx- 16 tmpsize)))
         (unsafe-bytes-copy! tmp tmpsize bs lo off)
         (set-xxh32-acc1! h (xxh32-round acc1 tmp  0))
         (set-xxh32-acc2! h (xxh32-round acc2 tmp  4))
         (set-xxh32-acc3! h (xxh32-round acc3 tmp  8))
         (set-xxh32-acc4! h (xxh32-round acc4 tmp 12))
         (set-xxh32-tmpsize! h 0)
         (set! lo off))
       (define limit
         (fx- end 16))
       (let loop ([acc1 (xxh32-acc1 h)]
                  [acc2 (xxh32-acc2 h)]
                  [acc3 (xxh32-acc3 h)]
                  [acc4 (xxh32-acc4 h)]
                  [lo lo])
         (cond
           [(fx<= lo limit)
            (loop (xxh32-round acc1 bs      lo    )
                  (xxh32-round acc2 bs (fx+ lo  4))
                  (xxh32-round acc3 bs (fx+ lo  8))
                  (xxh32-round acc4 bs (fx+ lo 12))
                  (fx+ lo 16))]
           [else
            (set-xxh32-acc1! h acc1)
            (set-xxh32-acc2! h acc2)
            (set-xxh32-acc3! h acc3)
            (set-xxh32-acc4! h acc4)
            (when (fx< lo end)
              (unsafe-bytes-copy! tmp 0 bs lo (fx+ lo (fx- end lo)))
              (set-xxh32-tmpsize! h (fx- end lo)))]))])))

(define (xxh32-digest h)
  (match-define (xxh32 len acc1 acc2 acc3 acc4 tmp tmpsize) h)
  (define acc
    (uint32+
     (if (>= len 16)
         (~> (uint32+
              (uint32rotl acc1 1)
              (uint32rotl acc2 7))
             (uint32+
              (uint32rotl acc3 12))
             (uint32+
              (uint32rotl acc4 18)))
         (uint32+ acc3 prime5))
     len))
  (xxh32-finalize acc tmp tmpsize))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (~> stx)
  (syntax-case stx ()
    [(_ (rator rand ...))
     #'(rator rand ...)]
    [(_ (rator0 rand0 ...) (rator rand ...) . rest)
     #'(~> (rator (rator0 rand0 ...) rand ...) . rest)]))

(begin-encourage-inline
  (define (xxh32-round acc bs lo)
    (~> (uint32 bs lo)
        (uint32* prime2)
        (uint32+ acc)
        (uint32rotl 13)
        (uint32* prime1)))

  (define (xxh32-finalize acc bs len)
    (let-values ([(acc pos) (xxh32-finalize-1 acc bs 0 len)])
      (let* ([acc (xxh32-finalize-2 acc bs pos len)]
             [acc (uint32xor acc (uint32shr acc 15))]
             [acc (uint32* acc prime2)]
             [acc (uint32xor acc (uint32shr acc 13))]
             [acc (uint32* acc prime3)]
             [acc (uint32xor acc (uint32shr acc 16))])
        acc)))

  (define (xxh32-finalize-1 acc bs pos len)
    (let loop ([acc acc]
               [pos pos]
               [len len])
      (if (fx>= len 4)
          (loop
           (~> (uint32 bs pos)
               (uint32* prime3)
               (uint32+ acc)
               (uint32rotl 17)
               (uint32* prime4))
           (fx+ pos 4)
           (fx- len 4))
          (values acc pos))))

  (define (xxh32-finalize-2 acc bs pos len)
    (let loop ([acc acc]
               [pos pos]
               [len (- len pos)])
      (if (fx> len 0)
          (loop
           (~> (unsafe-bytes-ref bs pos)
               (uint32* prime5)
               (uint32+ acc)
               (uint32rotl 11)
               (uint32* prime1))
           (fx+ pos 1)
           (fx- len 1))
          acc))))
