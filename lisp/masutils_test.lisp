#! /usr/local/bin/clisp

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
	(when (probe-file quicklisp-init)
		(load quicklisp-init)))

(ql:quickload "clunit")
(use-package :clunit)

(ql:quickload "cl-utilities")
;(shadowing-import 'cl-utilities:with-gensyms)
;(use-package :cl-utilities)
(load "masutils.lisp") ; includes package

(defsuite EverythingSuite ())

(defsuite ComparisonSuite (EverythingSuite))
(defsuite ListManipulationSuite (EverythingSuite))
(defsuite HashTableSuite (EverythingSuite))
(defsuite UtilitiesSuite (EverythingSuite))

(deftest equals-equals-test (ComparisonSuite)
  (assert-equal t (masutils:== 1 1))
  (assert-equal nil (masutils:== 1 2))
  (assert-equal t (masutils:== 1.1 1.1))
  (assert-equal nil (masutils:== 1.1 2.2))
  (assert-equal t (masutils:== "a" "a"))
  (assert-equal nil (masutils:== "a" "b"))
  (assert-equal t (masutils:== '("a" "b") '("a" "b")  ))
  (assert-equal nil (masutils:== '("a" "b") '("a" "c")  )))

(deftest not-equals-test (ComparisonSuite)
  (assert-equal nil (masutils:!= 1 1))
  (assert-equal t (masutils:!= 1 2))
  (assert-equal nil (masutils:!= 1.1 1.1))
  (assert-equal t (masutils:!= 1.1 2.2))
  (assert-equal nil (masutils:!= "a" "a"))
  (assert-equal t (masutils:!= "a" "b"))
  (assert-equal nil (masutils:!= '("a" "b") '("a" "b")  ))
  (assert-equal t (masutils:!= '("a" "b") '("a" "c")  )))

(deftest equals-equals-equals-test (ComparisonSuite)
  (let ((one 1) (a "a") (a2 "a") (arry1 '(1 2 3)) (arry2 '(1 2 3)))
    (assert-equal t (masutils:=== a a))
    (assert-equal nil (masutils:=== a a2))
    (assert-equal t (masutils:== arry1 arry2)) ; sanity check with == 
    (assert-equal nil (masutils:=== arry1 arry2))))


; RUN THE TEST
(setf clunit:*clunit-report-format* :default)
(print (run-suite 'EverythingSuite))
