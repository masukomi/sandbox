(module hello-world
  (hello)
  (import chicken)
  (import scheme)
  ; alternately: (import chicken scheme)
  (define (hello #!optional (name "World"))
      (string-append "Hello, " name "!"))
)
