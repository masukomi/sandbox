(defpackage :masutils
	(:use #:cl)
		(:export 
			; comparison tools
			:==
			:===
			:!=
			:boolit
			:char?
			:getenv-var
			:nil?
			:not-nil?
			:string?
			:empty?

			; utilities
			:println
			:get-shell-output
		))

(in-package :masutils)

;;; COMPARISON FUNCTIONS
(defmacro == (a b) `(equalp ,a ,b))
(defmacro != (a b) `(not (equalp ,a ,b)))
(defmacro === (a b) `(equal ,a ,b))

(defun boolit (var)
"converts a value into t or nil

### Returns: 
t or nil"
	(not (not var))
)

(defun char? (var)
"Tests if an object is a character
### Returns:
t or nil

### Paramaters:
* var - an object

### Note:
In many implementations you can just use eq to test
equality of characters with eq, but this is 
not guaranteed across systems. Thus the need 
for this method."
	(let ((type-result (values (type-of var))))
		(if (string= "SYMBOL" (type-of type-result))
			(not (not (string= "STANDARD-CHAR" type-result))))))

(defun nil? (var)
"Tells you if a variable is nil

### Returns: 
t or nil"
	(eq nil var)
)

(defun not-nil? (var)
"Tells you if a variable is not nil

### Returns: 
t or nil"
	(not (nil? var))
)

(defun empty? (var)
	(cond ((string= "SEQUENCE" (type-of var))
			(= (length var) 0))
		  ((string= "HASH-TABLE" (type-of var))
		  	(= (hash-table-size var) 0))
		  ((null var)
		  	t)
		  (t (error "(empty? var) Only supports SEQUENCE and HASH-TABLE objects"))))

(defun string? (var)
"Tests if an object is a string
### Returns:
t or nil

### Paramaters:
* var - an object"
	(let ((type-result (values (type-of var))))
		(if (string= "CONS" (type-of type-result))
			(not (not (string= "SIMPLE-BASE-STRING" (nth 0 (values type-result))))))))

;;;;;;;;; Other

(defun println (input)
"Sends the input to standard out followed by a newline

## Retuns:
nil

## Parameters
* input - a string "
	(format t "~A~A" input #\newline))


;TODO: return response code in addition to lines
;      will result in a more complicated data structure
;      but hey... :)
(defun get-shell-output (command &optional (args '()))
"Runs the specified shell program with the specified arguments

## Returns: the response lines as a list

## Parameters: 
* command - a string representing the command to run in the shell
* args - an optional list of arguments (strings) to pass to the command
"
(let (
       (in (ext:run-program command :arguments args :output :stream))
       (out '()))
  (when in
    (loop for line = (read-line in nil) 
      while line do (setf out (append out (list line)))
    )
    (close in))
  (return-from get-shell-output out)))

; from scheme's srfi-1
; "## Public: take (lst k)
; extracts the first k elements from the supplied list
; ### Parameters:
; * lst - a list
; * k - the number of elements you wish to extract
;
; ### Returns:
; The first k elements from the supplied list
;
; ### Notes: 
; If there are less than k elements in the list it will return
; a list with all the elements of lst + enough nils to make the
; returned list of the requested length.
;
; ### Example: 
;
;     (take '(1 2 3 4) 2 ) ;=> (1 2)
;     (take '(1 2) 4) ;=> (1 2 NIL NIL)"

(defun take (lst k)
  (if (eq 0 k)
      '()
      (cons (car lst)
            (take (cdr lst) (- k 1)))))

; also from srfi-1
(defun drop (lst k)
  (if (eq 0 k)
      lst
      (drop (cdr lst) (- k 1))))


; "## Public: split-by
; splits an list into multiple lists of n length.
;
; ### Parameters:
;  * n - the size of the lists it should be broken into.
;  * lst - the list to be split
;
; ### Returns:
; A list of smaller list of the specified length (or raises an exception).
; ### Examples:
;    (split-by 2 '(1 2 3 4)) ; => ((1 2) (3 4))
;    (split-by 2 '(1 2 3)) ; => ((1 2) (3)) ; not evenly divisible"

(defun split-by (n lst)
   (let ( (list-size (length lst)) )
     (if (not (eq 0 (mod list-size n)))
       (error (format nil "list is not evenly divisible by ~A: ~A" n lst)))
     (if (not (eq list-size 0))
         (cons (take lst n) (split-by n (drop lst n)))
         '() )))



;;;;;;;;; Command Line Helpers

(defun getenv-var (name &optional default)
  (let ((var 
    #+CMU
    (let ((x (assoc name ext:*environment-list*
                    :test #'string=)))
      (if x (cdr x) default))
    #-CMU
    (or
     #+Allegro (sys:getenv name)
     #+CLISP (ext:getenv name)
     #+CCL (getenv name)
     #+ECL (si:getenv name)
     #+SBCL (sb-unix::posix-getenv name)
     #+LISPWORKS (lispworks:environment-variable name))))
    ;; THUS ends the let assignment of "var"
    (if var
      var
      (if default default nil))
    ))

