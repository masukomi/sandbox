(module masutils
  (
   ; comparison tools
    ==
    !=
    ===
    !==
    boolify
    char?

    ; list manipulation
    sublist

    hash
    keys
    alist->hash
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

  (define (boolit x)
    (not (not x)))

  
  
  )
