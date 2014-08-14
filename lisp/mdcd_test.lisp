#! /usr/local/bin/clisp

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
	(when (probe-file quicklisp-init)
		(load quicklisp-init)))

(ql:quickload "clunit")
(use-package :clunit)

(ql:quickload "cl-utilities")
(load "mdcd-test-package.lisp")
(load "mdcd.lisp")


(defsuite EverythingSuite ())

(defsuite SupportFunctionsSuite (EverythingSuite))

(deftest line-matches-section-test (SupportFunctionsSuite)
  (assert-equal t (mdcd:line-matches-section? "## Foo" 0 :description))
  (assert-equal nil (mdcd:line-matches-section? "## Foo" 0 :parameters))
  (assert-equal t (mdcd:line-matches-section? "## Parameters" 5 :parameters))
  (assert-equal t(mdcd:line-matches-section? "## Parameters" 0 :parameters))
)

(deftest capture-good-line-test (SupportFunctionsSuite)
  ; (defun capture-good-line (line header-line section-found line-matches-section)
  (let ((test-line "test-line"))
    ; first matching header
    (assert-equal '("test-line") (mdcd:capture-good-line test-line t nil t))
    ; header after matching header
    (assert-equal nil (mdcd:capture-good-line test-line t t nil))
    ; non-header line, section found
    (assert-equal '("test-line") (mdcd:capture-good-line test-line nil t t))
    ; non-header line before matching section
    (assert-equal nil (mdcd:capture-good-line test-line nil nil t))))

(deftest string-is-md-header-test (SupportFunctionsSuite)
  (let ((yes "## I'm a header")
        (no "I'm not a header")
        (no-b " ## I'm not because of the space"))
    (assert-equal t (mdcd:string-is-md-header? yes))
    (assert-equal nil (mdcd:string-is-md-header? no))
    (assert-equal nil (mdcd:string-is-md-header? no-b))
        ))

(deftest find-header-lines-test (SupportFunctionsSuite)
  (let ((split-doc-string '("## Public: foo" 
                            "description" 
                            "### Parameters: "
                            "* this - this thing"
                            "* that - that thing"
                            "## Notes:"
                            "notes here")))
    (assert-equal '(0 2 5) (mdcd:find-header-lines split-doc-string))
                            ))
; RUN THE TEST
(setf clunit:*clunit-report-format* :default)
(print (run-suite 'EverythingSuite))
