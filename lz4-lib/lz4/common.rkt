#lang racket/base

(provide
 expect-byte
 expect-bytes)

(define (expect-byte who what in)
  (define b (read-byte in))
  (begin0 b
    (when (eof-object? b)
      (error who "unexpected EOF while reading ~a" what))))

(define (expect-bytes who what amt in)
  (define bs (read-bytes amt in))
  (begin0 bs
    (when (or (eof-object? bs)
              (< (bytes-length bs) amt))
      (error who "unexpected EOF while reading ~a" what))))
