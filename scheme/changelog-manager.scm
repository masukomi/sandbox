
;(use srfi-1)
(use srfi-13)
(import chicken)
(import scheme)
(import mdcd-config)
(import mdcd)
(import listicles)
(use json)

(define (get-user-input message)
  (print message)
  (read)
)

(define (ask-until-acceptable message display-options valid-responses)
  (let (( new-message (string-join (append (cons message '()) display-options) "\n")))
    (let ( (response (get-user-input new-message)))
      (if (list-includes? valid-responses response)
        response
        (ask-until-acceptable message display-options valid-responses)
      )
    )
  )
)

(define (get-change-type)
  (ask-until-acceptable "What kind of change?"
                                            '("1 - Fixed"
                                              "2 - Changed"
                                              "3 - Added")
                                            '(1 2 3))
)

(define (get-boolean-response message)
  (let ((response (ask-until-acceptable message '("y" "n") '("y" "n"))))
    (if (equal? response "y")
      #t
      #f
    )
  )
)

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
(if (get-boolean-response "Need Changelog?")
  (begin 
    ; (define change-type (nth (- (get-change-type) 1) '("Fixed" "Changed" "Added")))
    ; (define ticket (get-user-input "Which Ticket?"))
    ; (define url (get-user-input "Ticket URL?"))
    ; (define description (get-user-input "Describe your change:"))

    (define changelog-hash (make-hash-table))
    ; (hash-table-set! hash 'type change-type)
    ; (hash-table-set! hash 'ticket ticket)
    ; (hash-table-set! hash 'url url)
    ; (hash-table-set! hash 'description description)
    (json-write changelog-hash)
  )
  (print "Alas...")
)

(print "The End")



