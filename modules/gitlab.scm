(define-module (gitlab)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:use-module (gitlab gitlab)
  #:use-module (gitlab user)
  #:re-export (<gitlab>
               gitlab-token
               gitlab-client
               gitlab-debug-mode?
               gitlab-request-users
               gitlab-delete-user)
  #:export (gitlab-request-groups))



(define* (gitlab-request-groups gitlab
                                #:key
                                (id                #f)
                                (owned?            'undefined))
  (let ((query
         (make-sieved-list
           (cons-or-null 'owned owned?))))
    (if id
        (client-get (gitlab-client gitlab)
                    (format #f "/api/v4/groups/~a" id)
                    #:query query)
        (client-get (gitlab-client gitlab)
                    "/api/v4/groups/"
                    #:query query))))

;;; gitlab.scm ends here.
