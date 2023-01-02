#lang scribble/manual

@(require scribble/example
          (for-label file/xxhash32
                     racket/base
                     racket/contract))

@title{@tt{xxhash}}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

This package provides a pure-Racket implementation of xxHash32.

@section{Reference}
@defmodule[file/xxhash32]

@defproc[(xxhash32 [bs bytes?]
                   [seed (integer-in 0 #xFFFFFFFF) 0]) (integer-in 0 #xFFFFFFFF)]{

  Computes the XXH32 hash of @racket[bs] with the given @racket[seed].
}

@deftogether[(
  @defproc[(xxh32? [v any/c]) boolean?]
  @defproc[(make-xxh32 [seed (integer-in 0 #xFFFFFFFF) 0]) xxh32?]
  @defproc[(xxh32-reset! [h xxh32?]
                         [seed (integer-in 0 #xFFFFFFFF) 0]) void?]
  @defproc[(xxh32-update! [h xxh32?]
                          [bs bytes?]
                          [start exact-nonnegative-integer? 0]
                          [end exact-nonnegative-integer? (bytes-length bs)]) void?]
  @defproc[(xxh32-digest [h xxh32?]) (integer-in 0 #xFFFFFFFF)]
)]{

  The @racket[make-xxh32] procedure returns a hashing context that can
  be used to compute an XXH32 digest using constant memory.

  @examples[
    (require file/xxhash32)
    (define ctx (make-xxh32))
    (xxh32-update! ctx #"hello")
    (xxh32-update! ctx #" world!")
    (xxh32-digest ctx)
  ]
}
