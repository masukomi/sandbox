#!/usr/local/bin/csi
; (require-extension test)
(use test); ^^ same effect
(import hello-world)

;; Simple test

;; group
(test-group "Parameterized Hello World"
  (test "Called with no args returns hello world" 
               "Hello, World!" (hello))
  (test "Called with an arg returns hello arg" 
               "Hello, Arg!" (hello "Arg")))


;; IMPORTANT! The following ensures nightly automated tests can
;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)

