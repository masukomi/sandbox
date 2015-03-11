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

(deftest range-test (UtilitiesSuite)
  (assert-equal '(0 1 2 3 4) (masutils:range 5))
  (assert-equal '(2 3 4 5 6) (masutils:range 5 2))
  (assert-equal '(0 2 4 6 8) (masutils:range 5 0 2))
  (assert-equal '(0 -2 -4 -6 -8) (masutils:range 5 0 -2))
)

(deftest hash-test (HashTableSuite)
  (let ((myhash (make-hash-table)))
       (masutils:hash myhash :foo :bar) ; add an entry
       (assert-equal 1 (hash-table-count myhash))
       (assert-equal :BAR (masutils:hash myhash :foo)) ; retrieve an entry
       (masutils:hash myhash :foo :baz) ; change an entry
       (assert-equal :BAZ (masutils:hash myhash :foo)) ; retrieve an entry
       ))

(deftest alist->hash-test (HashTableSuite)
  (let* ((alist '((:foo . :bar ) (:baz . :bumble)) )
         (new-hash (masutils:alist->hash alist)))
       (masutils:println new-hash)
       (assert-equal 2 (hash-table-count new-hash ))
       (assert-equal :bar (gethash :foo new-hash))
       (assert-equal :bumble (gethash :baz new-hash))
       
  )
)

; RUN THE TEST
(setf clunit:*clunit-report-format* :default)
(print (run-suite 'EverythingSuite))
