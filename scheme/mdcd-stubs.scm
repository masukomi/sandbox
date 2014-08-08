;# MCDC Stubs
; This file contains stub methods which do nothing,
; but allow your code to compile. 
; The MCDC generator script will use the full (non-stub)
; versions of these methods which will, in turn, 
; generate your documentation.

; # Dependencies
; directory-utils
(use directory-utils)

(define (set-mdcd-home file-path)
  (define *mdcd-home* file-path))

(define (get-mdcd-home)
  ; FIXME can't reference undefined var
  ; can't test if var is defined.
  (cond ((null? *mdcd-home*) (values "~/mdcd/scheme/"))
        (else (values *mdcd-home*))))

; ## Public:
; Gets the documentation for the specified key
; ### Parameters:
; * key - the name of the method/variable/syntax you want 
;   documentation for.
(define (get-doc key)
  ; WARNING: THE FOLLOWING IS INCORRECT! Dunno right syntax
  (hash-table-ref/default *mdcd-reference* key "Undocumented"))

; ## Public:
; Generates documentation for a function.
;
; ### Paramaters:
; * name - a symbol representing the name of the function
; * doc-string - a markdown string documenting the function
; 
; ### Returns:
; Returns no values, this is just a stub.
(define (doc-fun name doc-string)
  (
  (hash-table-set! *mdcd-reference* (format "~s" name) doc-string))

; ## Public:
; Generates documentation for a syntax change.
;
; ### Paramaters:
; * mini-syntax-identifier - a small example of the resulting changes
; * doc-string - a markdown string documenting the function
; 
; ### Returns:
; Returns no values, this is just a stub.
; 
; ### Notes:
; Picking a good `mini-syntax-identifier` is tricy because syntax changes
; typically don't have some standard symbol you can point to.
; If, for example you were to add Ruby style array initialization 
; syntax (e.g. ["a", "b"] ) you might choose `[...]` as your 
; `mini-syntax-identifier`. Just make an attempt to come as close to something
; referencable (like a method name) as possible. 
(define (doc-syntax mini-syntax-identifier doc-string)
  (values))

; ## Public:
; Generates documentation for a variable.
; Typically you would only use this for a variable of atypical significance
; that others should be made aware of 
;
; ### Paramaters:
; * name - a symbol representing the name of the variable
; * doc-string - a markdown string documenting the variable
; 
; ### Returns:
; Returns no values, this is just a stub.
(define (doc-var name doc-string)
  (values)) ;as close as I know how to get to a no-op in scheme
