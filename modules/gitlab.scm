(define-module (gitlab)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:use-module (gitlab session)
  #:use-module (gitlab api user)
  #:re-export (<session>
               gitlab-session-token
               gitlab-session-client
               gitlab-session-debug-mode?
               gitlab-api-users-get
               gitlab-api-users-delete)
  #:export (gitlab-request-groups))



(define* (gitlab-request-groups session
                                #:key
                                (id                #f)
                                (owned?            'undefined))
  (let ((query
         (make-sieved-list
           (cons-or-null 'owned owned?))))
    (if id
        (client-get (gitlab-session-client session)
                    (format #f "/api/v4/groups/~a" id)
                    #:query query)
        (client-get (gitlab-session-client session)
                    "/api/v4/groups/"
                    #:query query))))

;;; gitlab.scm ends here.
