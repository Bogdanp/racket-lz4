#lang racket/base

(require ffi/unsafe
         file/xxhash32
         rackcheck
         rackunit)

(define-check (check-digest bs expected)
  (define h32 (xxhash32 bs))
  (unless (= h32 expected)
    (with-check-info
      (['digest h32]
       ['expected expected])
      (fail-check))))

(check-digest #"" #x2cc5d05)
(check-digest #"1" #xb6ecc8b2)
(check-digest #"12" #xd43589af)
(check-digest #"123" #xb6855437)
(check-digest #"1234" #x1543429)
(check-digest #"abcd" #xa3643705)
(check-digest #"12345" #xb30d56b4)
(check-digest #"123456" #xb7014066)
(check-digest #"1234567" #xdd8d554e)
(check-digest #"12345678" #x89f05aa5)
(check-digest #"123456789" #x937bad67)
(check-digest #"123456789A" #xee4c2232)
(check-digest #"123456789AB" #x525ebf88)
(check-digest #"123456789ABC" #x4c91c2e5)
(check-digest #"123456789ABCD" #x772609a4)
(check-digest #"123456789ABCDE" #xde40edc)
(check-digest #"123456789ABCDEF" #x576e3cf9)
(check-digest #"123456789ABCDEF1" #x82d80129)
(check-digest #"123456789ABCDEF12" #x4689504)
(check-digest #"123456789ABCDEF123" #xb6786140)
(check-digest #"123456789ABCDEF1234" #xc33e9edc)
(check-digest #"123456789ABCDEF12345" #x8cc12eb4)
(check-digest #"123456789ABCDEF123456" #xf28177f6)
(check-digest #"123456789ABCDEF1234567" #xcf887691)
(check-digest #"123456789ABCDEF12345678" #x7b1af730)
(check-digest #"123456789ABCDEF123456789" #x1f34766d)
(check-digest #"123456789ABCDEF123456789A" #x772d203c)
(check-digest #"123456789ABCDEF123456789AB" #x30967301)
(check-digest #"123456789ABCDEF123456789ABC" #x2b595fa)
(check-digest #"123456789ABCDEF123456789ABCD" #xd09d9fe)
(check-digest #"123456789ABCDEF123456789ABCDE" #xdacc797)
(check-digest #"123456789ABCDEF123456789ABCDEF" #x2f375968)
(check-digest #"123456789ABCDEF123456789ABCDEF1" #xff6d43a9)
(check-digest #"123456789ABCDEF123456789ABCDEF12" #x852d687c)
(check-digest #"123456789ABCDEF123456789ABCDEF123" #xef78a638)
(check-digest #"123456789ABCDEF123456789ABCDEF1234" #xb8939d98)
(check-digest #"123456789ABCDEF123456789ABCDEF12345" #x83a9e3bc)
(check-digest #"123456789ABCDEF123456789ABCDEF123456" #xb3b65d47)
(check-digest #"123456789ABCDEF123456789ABCDEF1234567" #x929b9cc4)
(check-digest #"123456789ABCDEF123456789ABCDEF12345678" #xe438dfb8)
(check-digest #"123456789ABCDEF123456789ABCDEF123456789" #xed8d024f)
(check-digest #"123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF" #xdfc3325c)
(check-digest #"123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF" #x1fb5b995)
(check-digest #"123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF" #xd57f0bc7)
(check-digest (make-bytes (* 100 1024) 255) #x81e25350)

(let ([h (make-xxh32)])
  (xxh32-update! h #"1")
  (check-equal? (xxh32-digest h) #xb6ecc8b2)
  (xxh32-update! h #"2")
  (check-equal? (xxh32-digest h) #xd43589af)
  (xxh32-update! h #"3456789ABCDEF")
  (check-equal? (xxh32-digest h) #x576e3cf9)
  (xxh32-update! h #"1")
  (check-equal? (xxh32-digest h) #x82d80129))

(define oracle-lib
  (ffi-lib "/opt/local/lib/libxxhash.dylib" #:fail (λ () #f)))
(when oracle-lib
  (define oracle
    (get-ffi-obj "XXH32" oracle-lib (_fun _bytes _size _uint32 -> _uint32)))
  (check-equal?
   (oracle #"hello" 5 0)
   (xxhash32 #"hello"))
  (check-property
   (property ([bs (gen:bytes)]
              [seed (gen:integer-in 0 #x7FFFFFFF)])
     (check-equal?
      (oracle bs (bytes-length bs) seed)
      (xxhash32 bs seed)))))
