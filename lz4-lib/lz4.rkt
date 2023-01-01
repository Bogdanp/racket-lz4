#lang racket/base

(require "lz4/buffer.rkt"
         "lz4/frame.rkt")

(provide
 lz4-decompress-through-ports)

(define (lz4-decompress-through-ports in out)
  (let loop ()
    (read-frame! in out)
    (unless (eof-object? (peek-byte in))
      (loop))))
