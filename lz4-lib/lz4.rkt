#lang racket/base

(require "lz4/frame.rkt")

(provide
 lz4-decompress-through-ports)

(define (lz4-decompress-through-ports in out #:validate-content? [validate-content? #f])
  (let loop ()
    (read-frame! in out #:validate-content? validate-content?)
    (unless (eof-object? (peek-byte in))
      (loop))))
