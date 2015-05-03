; Used for testing the solutions
(load "asserts.scm")

;; P1. Find the last box of a list
(define (my-last lst)
  (cond ((null? (cdr lst)) (car lst))
	(else (my-last (cdr lst)))))

(assertEq
 'c
 (my-last '(a b c)))

;; P2. Find the last but one box of a list
(define (last-but-one lst)
  (cond ((null? (cddr lst)) (car lst))
	(else (last-but-one (cdr lst)))))

(assertEq
 'b
 (last-but-one '(a b c)))

;; P3. Find the K'th element of a list.

(define (kth k lst)
  (cond ((zero? k) (car lst))
	(else (kth (- k 1) (cdr lst)))))

(assertEq
 'c
 (kth 2 '(a b c)))

;; P4. Find the number of elements of a list.

(define (number-of-elements lst)
  (cond ((null? lst) 0)
	(else (+ 1 (number-of-elements (cdr lst))))))

(assertEq
 5
 (number-of-elements '(a b c d e)))

(assertEq 
 0 
 (number-of-elements '()))

;; P5. Reverse a list.

(define (reverse-list xs)
  (fold-left (lambda (x y) (cons y x)) '() xs))

(assertEq
 '(e d c b a)
 (reverse-list '(a b c d e)))

(assertEq
 '()
 (reverse-list '()))

;; P6. Find out whether a list is a palindrome.

(define (is-palindrome x)
  (equal? x (reverse-string x)))

(assertTrue (is-palindrome "abba"))
(assertTrue (is-palindrome ""))
(assertTrue (is-palindrome "WTF FTW"))
(assertFalse (is-palindrome "not a palindrome"))

;; P7. Flatten a nested list structure.

(define (flatten lst)
  (define (merge next acc)
    (cond ((list? next) (append acc (flatten next)))
	  (else (append acc (list next)))))
  (fold-left merge '() lst))

(assertEq
 '()
 (flatten '()))

(assertEq
 '(1 2 3)
 (flatten '(1 2 3)))

(assertEq
 '(1 2 3)
 (flatten '(1 2 (3))))

(assertEq
 '(a b c d e f g h)
 (flatten '(a b (c (d) e) (f g) h)))

;; P8. Eliminate consecutive duplicates of list elements.

(define (remove-dups lst)
  (fold-right (lambda (next acc) 
		(cond ((null? acc) (cons next acc))
		      ((equal? next (car acc)) acc)
		      (else (cons next acc))))
	      '()
	      lst))

(assertEq '() (remove-dups '()))

(assertEq
 '(a b c d)
 (remove-dups '(a a b c c c d)))

(assertEq
 '(a b a b)
 (remove-dups '(a a a b a a b b b)))

;; P9. Pack consecutive duplicates of list elements into sublists.

(define (pack-dups lst)
  "todo")

(assertEq '() (pack-dups '()))

(dont assertEq
 '(a (b b b) c)
 (pack-dups '(a b b b c)))

;; P10. Run-length encoding of a list.
;; P11. Modified run-length encoding.
;; P12. Decode a run-length encoded list.
;; P13. Run-length encoding of a list (direct solution).
;; P14. Duplicate the elements of a list.
;; P15. Replicate the elements of a list a given number of times.
;; P16. Drop every N'th element from a list.
