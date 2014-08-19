; WARNING: DO NOT USE THIS FILE IN PRODUCTION
; YES, best practice says only test the methods
; exposed by the API. HOWEVER, I want to guarantee
; I get this right. So, this package definition
; exposes internal methods so that they can be 
; tested via clunit.
; 
; The methods noted below as being private and 
; exposed just for testing should *not* be relied 
; upon and may change or be deleted without warning. 

(defpackage :mdcd
	(:use #:cl #:cl-utilities)
	(:export
      :doc
      :show-doc
      :set-mdcd-home
      :get-mdcd-home
      :path-for

      ; PRIVATE METHODS EXPOSED JUST FOR TESTING
      :line-matches-section?
      :capture-good-line
      :find-header-lines
      :string-is-md-header?
      :mdcd-file-for
    ))
