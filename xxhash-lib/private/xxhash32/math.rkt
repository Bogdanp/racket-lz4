#lang racket/base

(provide (all-defined-out))

(define u32max #xFFFFFFFF)
(define u32max+1 (add1 u32max))

(define (uint32 bs pos)
  (integer-bytes->integer bs #f #f pos (+ pos 4)))

(define (uint32+ a b)
  (define v (+ a b))
  (if (> v u32max)
      (- v u32max 1)
      v))

(define (uint32- a b)
  (define v (- a b))
  (if (< v 0)
      (+ u32max+1 v)
      v))

(define (uint32* a b)
  (remainder
   (* (remainder a u32max+1)
      (remainder b u32max+1))
   u32max+1))

(define (uint32rotl a b)
  (define v (arithmetic-shift a b))
  (bitwise-ior
   (bitwise-and v u32max)
   (bitwise-and (arithmetic-shift v -32) u32max)))

(define (uint32shr a b)
  (arithmetic-shift (bitwise-and a u32max) (- b)))

(define (uint32xor a b)
  (bitwise-xor a b))
