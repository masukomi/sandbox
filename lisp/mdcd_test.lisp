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
  (assert-equal t (mdcd:line-matches-section? "## Foo" 1 :description))
  (assert-equal nil (mdcd:line-matches-section? "## Foo" 1 :parameters))
  (assert-equal t (mdcd:line-matches-section? "## Parameters" 6 :parameters))
  (assert-equal t(mdcd:line-matches-section? "## Parameters" 1 :parameters))
)

; RUN THE TEST
(setf clunit:*clunit-report-format* :default)
(print (run-suite 'EverythingSuite))
