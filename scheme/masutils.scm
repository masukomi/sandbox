(module masutils
  (
   ; comparison tools
    ==
    !=
    ===
    !==
    boolify
    get-type
    )

  (import chicken)
  (import scheme)

  (define (== a b)
    (equal? a b))

  (define (!= a b)
    (not (== a b)))

  (define (=== a b)
    (eq? a b))
  
  (define (!== a b)
    (not (=== a b)))

  (define (boolify x)
    (not (not x)))

  (define (get-type x)
    (cond 
      ((number? x)   'number)
      ((list? x)     'list)
      ((pair? x)     'pair)
      ((string? x)   'string)
      ((boolean? x)  'boolean)
      ((null? x)     'null)
      ((symbol? x)   'symbol)
      ((char? x)     'character)
      ((vector? x)   'vector)
      ((procedure? x)'procedure)
      (else          'unknown)))
  )

