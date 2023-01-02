#lang racket/base

(#%declare #:unsafe)

(require (for-syntax racket/base)
         racket/performance-hint
         racket/require
         racket/unsafe/ops
         (filtered-in
          (λ (name)
            (and (regexp-match #rx"^unsafe-fx" name)
                 (regexp-replace #rx"unsafe-" name "")))
          racket/unsafe/ops))

(provide (all-defined-out))

(define u32max #xFFFFFFFF)
(define u32max+1 (add1 u32max))

(begin-encourage-inline
  (define (uint32 bs pos)
    (fxior
     (unsafe-bytes-ref bs pos)
     (fxlshift (unsafe-bytes-ref bs (fx+ pos 1)) 8)
     (fxlshift (unsafe-bytes-ref bs (fx+ pos 2)) 16)
     (fxlshift (unsafe-bytes-ref bs (fx+ pos 3)) 24)))

  (define (uint32+ a b)
    (define v
      (fx+ a b))
    (if (fx> v u32max)
        (fx- v u32max 1)
        v))

  (define (uint32- a b)
    (define v
      (fx- a b))
    (if (fx< v 0)
        (fx+ u32max+1 v)
        v))

  (define (uint32* a b)
    (cond
      [(fx= b 0) 0]
      [(fx= b 1) a]
      [(fx> a (fxquotient u32max b))
       (define a0 (fxand a #xFFFF))
       (define a1 (fxrshift a 16))
       (define b0 (fxand b #xFFFF))
       (define b1 (fxrshift b 16))
       (define m0 (fx* a0 b0))
       (define m1 (fx+ (fx* a1 b0) (fxrshift m0 16)))
       (define mc (fx+ (fx* a0 b1) (fxand m1 #xFFFF)))
       (define r0 (fx+ (fxlshift mc 16) (fxand m0 #xFFFF)))
       (fxremainder r0 u32max+1)]
      [else
       (fx* a b)]))

  (define (uint32rotl a b)
    (define d (fx- 32 b))
    (define m0 (fx- (fxlshift 1 d) 1))
    (define m1 (fx- (fxlshift 1 b) 1))
    (fxior
     (fxlshift (fxand a m0) b)
     (fxand (fxrshift a d) m1)))

  (define uint32shr fxrshift)
  (define uint32xor fxxor))
