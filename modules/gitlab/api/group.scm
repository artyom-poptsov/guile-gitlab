(define-module (gitlab api group)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:use-module (gitlab session)
  #:export (gitlab-api-group-get))



(define* (gitlab-api-groups-get session
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

;;; group.scm ends here.
