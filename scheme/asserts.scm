(define (assertEq a b)
  (if (not (equal? a b))
      (error a "<>" b)))

(define (assertTrue b)
  (assertEq #t b))

(define (assertFalse b)
  (assertEq #f b))

(define (dont . args)
  "ignoring")
