#lang racket/base

(require "lz4/buffer.rkt"
         "lz4/frame.rkt")

(provide
 lz4-decompress-through-ports)

(define (lz4-decompress-through-ports in out)
  (define buf (make-buffer))
  (let loop ()
    (read-frame! buf in)
    (copy-buffer out buf)
    (unless (eof-object? (peek-byte in))
      (buffer-reset! buf)
      (loop))))
