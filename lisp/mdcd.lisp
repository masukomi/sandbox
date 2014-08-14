; # MCDC Mardown Code DoCs
; Allows you to markup your code with Markdown.
; Enables you to: 
; * request docs for any documented function in a REPL
; * generate markdown files for easy searchability 
;   via tools like spotlight
; * generate static html for distributing your documentation
;
; # Dependencies
; * cl-utilities

; two package files for this
; mdcd-package.lisp (the real one)
; mdcd-test-package.lisp (the one for testing with everything exported)

(in-package :mdcd)

(shadowing-import 'cl-utilities:with-gensyms)
(use-package :cl-utilities)

;(require-extension regex)

; ## Private: *mcdc-home*
; We just need to set this because Chicken Scheme
; has no mechanism for testing if a variable has 
; been defined or not. It can be overridden via the
; `set-mcdc-home` method.
(defparameter *mdcd-home* 
  (append (cdr (split-sequence #\/ (ext:getenv "HOME" ))) '("mdcd" "lisp")))


; ## Public: set-mcdc-home
; Sets the directory where MCDC files are stored
; 
; ### Parameters:
; * directory-list a list of directories.
;   E.g.: '("home" "current_username")
; 
; ### Returns: 
; The path to the directory it will save files to.
;
(defun set-mdcd-home (directory-list)
  (defparameter *mdcd-home* directory-list)
        (get-mdcd-home))
        ; (namestring (get-mdcd-home)) => converted to string

; ## Public: get-mdcd-home
; Returns the path to the directory where MDCD files will be stored.
; 
; ### Returns:
; The path to the directory where MDCD files will be stored.
; 
; ### Note:
; Defaults to $HOME/mdcd/scheme/
(defun get-mdcd-home ()
  (make-pathname :directory `(:absolute ,@*mdcd-home*)))

; ## Private: mdcd-name-cleaner
; converts method and variable names into something more filesystem friendly
; ### Parameters:
; * name - the name to be cleaned.
; ### Returns:
; The "cleaned" name.
(defun mdcd-name-cleaner (name)
  ; "[^\\w_\-]+" seemed like it was too likely to result
  ; in overlap. I could hash the name but I would prefer
  ; to keep the files as something meaningful to humans.
  (let ((cleaned name))
    (loop for bad-char in '(#\: #\/ #\* #\| #\+)
      do (setf cleaned (substitute #\_ bad-char cleaned))
      )
    (return-from mdcd-name-cleaner cleaned)))

; ## Public: mdcd-file-for
; Provides the file-path for a given identifier. 
; ### Paramaters:
; * identifier - the function/variable/syntax you want documentation for.
; * subfolder - This is typically the grouping from doc
; * item-type - see the "doc" function for details on the item-type param
; ### Returns:
; The file-path where the identifiers docs should be written / found.

(defun mdcd-file-for (identifier &optional (subfolder "") (item-type ""))
  (make-pathname :directory `(:absolute 
                              ,@*mdcd-home* 
                              ,subfolder
                              ,item-type)
                              :name
                              (mdcd-name-cleaner identifier)
                              :type  "md"))

(defun mdcd-ensure-dir (a-pathname)
  (ensure-directories-exist 
      (make-pathname :directory 
        (pathname-directory a-pathname))))


; ## Private: mdcd-write-doc
; Writes the specified doc-string to the specified file-path
;
; ### Paramaters:
; * doc-string - the string of documentation
; * a-pathname - the pathname where the filename that should be created / replaced
;
; ### Returns:
; Returns true if successful
(defun mdcd-write-doc (doc-string a-pathname )
  (mdcd-ensure-dir a-pathname)
  (let ((stream 
                (open a-pathname
                      :direction :output :if-exists :supersede)))
      (write-string doc-string stream)
      (close stream)))


(defun path-for (name item-type grouping)
  (mdcd-file-for 
    (string-downcase (symbol-name name)) 
    (cond ((not grouping) "")
          (t (string-downcase (symbol-name grouping))))
    (string-downcase (symbol-name item-type))
    ))

  

(defun doc (name doc-string 
  &optional &key (item-type :function) (grouping nil))
"## Public: doc
Generates and saves documentation.

### Paramaters:
* name - a symbol representing the name of the function
* doc-string - a markdown string documenting the function
* Optional Named Parameters
  * item-type - A symbol. Defaults to :function but can be any of the following: 
    :function, :variable, :macro, :class, :meta. :meta is for use in documenting
    less specific things, like notes about the package. 
  * grouping - A symbol. Defaults to nil otherwise is a custom grouping, 
    typically a package name under which to collect a set of documentation.

### Returns:
The doc-string passed in

### Note:
The item-type serves to help segregate the stored files. "
 ; we can't set the documentation directly 
 ; because we want to use this in place of the standard 
 ; docstring , which means the method wouldn't exist yet
 ; to have its documentation set.
 ; instead we'll just use print
 ;(setf (documentation name 'function) doc-string)
 (let ((file-path (path-for name item-type grouping)))
        (mdcd-write-doc doc-string file-path)
        file-path)
        doc-string)
        

; We now have enough code to start eating our own dog food.
; YAY.
; Alas, there's nothing left that needs documenting.

(defun show-doc (name &optional &key (item-type :function) (grouping nil))
  (doc 'show-doc "## Public: show-doc
Loads and displays the documentation for the specified object.

### Parameters:
* name - The name of the function/variable/thing you're seeking docs for.
* Optional *Named* Parameters:
  * item-type - A symbol. Defaults to :function but can be any of the following: 
    :function, :variable, :macro, :class, :meta.
  * grouping -  A symbol. Defaults to nil otherwise is a custom grouping, 
    typically a package name under which to collect a set of documentation.

### Returns:
A string containing the documention requested or an indication of 
where it attempted to find it.
" :grouping "mdcd")
 (let ((file-path (path-for name item-type grouping)))
      (if (probe-file file-path)
        (progn 
          (let ((in (open file-path)) (response '())  )
            (setf response (append response `(,(read-line in))))
            (close in)
            (return-from show-doc (format nil "~{~A~^~% ~}" response))))
        (format nil "No Docs Found at ~A" file-path))))

; unit tested
(defun line-matches-section? (line line-number section)
  (if (and (equal 0 line-number) (equal section :description))
      (return-from line-matches-section? t)
      (and (not (not (search 
                      (symbol-name section)
                      ; upcasing line because symbol-name returns upcase string
                      (string-upcase line)))))))

(defun capture-good-line (line header-line section-found line-matches-section)
  (cond 
    ; line in wrong section
    ((and (not section-found) (not header-line)) 
      (return-from capture-good-line nil)) ; do nothing
    ; line in section we do want
    ((and section-found (not header-line))
      (return-from capture-good-line `(,line)))
    ; header of correct section
    ((and (not section-found) header-line) line-matches-section 
            (return-from capture-good-line `(,line) )
            )
    ; header of NEXT section (we don't want it)
    ((and section-found header-line)
      (return-from capture-good-line nil))
    (t (return-from capture-good-line nil)))) ; can't happen... i hope

(defun string-is-md-header? (a-string)
  (equal 0 (position #\# a-string)))

(defun find-header-lines (lines &optional (line-num -1))
  (map 'list (lambda (y) (car y)) ; get the line numbers from dotted pairs
    (remove-if-not (lambda (x) (cdr x)) ; remove non-dotted pairs
      ; generate a list of (line-num . t) and (line-num) entries
      (cond
        ((> (length lines) 1)
          (loop for i upto (length lines)
                for line = (nth i lines)
                collect (cons i (string-is-md-header? line))))
        (t `(,(cons (+ 1 line-num) (string-is-md-header? (car lines))))))
      )))

;   (doc 'extract-section "## Private: extract-section
; Extracts the specified section of a given doc string (if available).
;
; ### Parameters:
; * section - A symbol: any of the following: 
;   :description, :parameters, :returns, :notes, :examples. :description will 
;   extract the first section which hopefully includes the name, visibility,
;   and overview.
; * doc-string - The doc string from which to attempt the extraction 
;   of the specified section
; ### Returns: 
; The extracted section of the docs." :grouping "mdcd")

(defun extract-section (section doc-string)
  (let* ((lines (split-sequence #\newline doc-string))
        (response '())
        (header-line-nums (find-header-lines lines))
        ) ;end let vars
    (do ((remaining-header-lines header-line-nums (cdr remaining-header-lines))
          (line-number (car header-line-nums) (car remaining-header-lines))
        )
      (nil)                       ;Do forever(ish).
      (if line-number
          (let ((line (nth line-number lines))
                (next-line (nth (+ 1 line-number) lines))
                (more-headers (> (length remaining-header-lines) 0))
                )
            (if (line-matches-section? line line-number section) 
                 (return-from extract-section
                    (if (and 
                          next-line ; if this is not the last line
                          more-headers) ; and there's a header after 
                          (loop for i from line-number to (- (nth 1 remaining-header-lines) 1) collect (nth i lines))
                          (loop for i from line-number to (- (length lines) 1) collect (nth i lines))))))
          (return nil)))))
