; CHICKEN SCHEME VERSION

(use srfi-1) ; iota
(use srfi-69) ; hash-table
(use loops)
(load "mdcd-stubs.scm")

(doc-fun 'split-by "## Public: 
splits an list into multiple lists of n length (or smaller).
### Parameters:
 * n - the size of the lists it should be broken into.
 * lst - the list to be split
   If the provided list can not be evenly divisible
   by n then the last returned list will contain
   the remaining elements.
### Returns:
A list of smaller list of the specified length (or smaller).
### Examples:
   (split-by 2 '(1 2 3 4)) ; => ((1 2) (3 4))
   (split-by 2 '(1 2 3)) ; => ((1 2) (3)) ; not evenly divisible")
(define (split-by n lst)
   (let ( (list-size (length lst)) )
     (if (not (eq? 0 (modulo list-size n)))
       (error (sprintf "list is not evenly divisible by ~A: ~A" n lst)))
     (if (not (eq? list-size 0))
         (cons (take lst n) (split-by n (drop lst n)))
         '() )))

; ## Public: 
; returns the "nth" item of a list
; ### Parameters:
; * n - the 0 based index into the list
; * lst - the list you wish to receive the nth element from
; ### Returns:
; The nth item of a list (if available)
; ### Exceptions:
; If n is outside of the bounds of the list then an 
; "Index out of bounds" error will be thrown"
(define (nth n lst)
  (if (or (> n (length lst)) (< n 0))
    (error 'nth "Index out of bounds.")
    (if (eq? n 0)
      (car lst)
      (nth (- n 1) (cdr lst)))))

; ## Public:
; Enables support for Ruby style array (list) instantiation.
; # Examples:
;     ["a" "b"] ; => ("a" "b")
; # Notes: 
; No, this really doesn't save you much. The same could
; be done with `'("a" "b")` but the square brackets make
; it clearer that you're dealing with a simple array.
; 
; This is especially useful when your array looks like
; `'(foo bar)` which, at a quick glance of code might 
; look like a form to be executed rather than two 
; elements of a list: `[foo bar]` is non-ambiguous.
(set-read-syntax! #\[
  (lambda (port)
    (let loop ((c (peek-char port)) (exps '()))
      (cond  ((eof-object? c)
              (error "EOF encountered while parsing [ ... ] clause"))
             ((char=? c #\])
              (read-char port) ; discard
              `(list ,@(reverse exps))) ; they've been built up backwards
             ((char-whitespace? c)
              (read-char port) ; discard whitespace
              (loop (peek-char port) exps))
             (else
              (let ((exp (read port)))
                (loop (peek-char port)
                      (cons exp exps))))))))
; ## Public:
; Enables support for Ruby(ish) style hash instantiation.
; # Examples:
;     {"a" 1 "b" 2} ; => hash-table with keys "a" and "b"
;                   ; each having respective values of 1 and 2
; # Notes: 
; Because hash-tables syntax sucks in lisp/scheme.
(set-read-syntax! #\{
   (lambda (port)
     (let loop ((c (peek-char port)) (exps '()))
       (cond ((eof-object? c)
              (error "EOF encountered while parsing { ... } clause"))
             ((char=? c #\})
              (read-char port)   ; discard
              `(alist->hash-table  '( ,@(split-by 2 (reverse exps))) ))
             ((char-whitespace? c)
              (read-char port)   ; discard whitespace
              (loop (peek-char port) exps))
             (else
              (let ((exp (read port)))
                (loop (peek-char port)
                      (cons exp exps))))))))


