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

(begin-encourage-inline
  (define (uint32 bs pos)
    (fxior
     (unsafe-bytes-ref bs pos)
     (fxlshift (unsafe-bytes-ref bs (fx+ pos 1)) 8)
     (fxlshift (unsafe-bytes-ref bs (fx+ pos 2)) 16)
     (fxlshift (unsafe-bytes-ref bs (fx+ pos 3)) 24)))

  (define (uint32+ a b)
    (fxand (fx+/wraparound a b) #xFFFFFFFF))

  (define (uint32- a b)
    (fxand (fx-/wraparound a b) #xFFFFFFFF))

  (define (uint32* a b)
    (fxand (fx*/wraparound a b) #xFFFFFFFF))

  (define (uint32rotl a b)
    (define d (fx- 32 b))
    (define m0 (fx- (fxlshift 1 d) 1))
    (define m1 (fx- (fxlshift 1 b) 1))
    (fxior
     (fxlshift (fxand a m0) b)
     (fxand (fxrshift a d) m1)))

  (define uint32shr fxrshift)
  (define uint32xor fxxor))
