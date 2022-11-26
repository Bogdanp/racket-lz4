#lang scribble/manual

@(require (for-label file/lz4
                     racket/base))

@title{LZ4}
@defmodule[file/lz4]

This module provides a pure-Racket decompressor for LZ4-compressed
in accordance with the LZ4 frame@cite{LZ4_FRAME} and
block@cite{LZ4_BLOCK} formats.

@defproc[(lz4-decompress-through-ports [in  input-port?]
                                       [out output-port?]) void?]{

  Reads LZ4-compressed frames from @racket[in] and writes the
  uncompressed data to @racket[out].  If the compressed data is
  corrupted or contains unsupported features, raises an
  @racket[exn:fail] error.

  Checksums are not verified.  Do not decompress untrusted input.
}

@bibliography[
  @bib-entry[
    #:key "LZ4_BLOCK"
    #:title "LZ4 Block Format"
    #:url "https://github.com/lz4/lz4/blob/8a31e6402df11c1bf8fbb1db3b29ec2c76fe6f26/doc/lz4_Block_format.md"
  ]

  @bib-entry[
    #:key "LZ4_FRAME"
    #:title "LZ4 Frame Format"
    #:url "https://github.com/lz4/lz4/blob/8a31e6402df11c1bf8fbb1db3b29ec2c76fe6f26/doc/lz4_Frame_format.md"
  ]
]
