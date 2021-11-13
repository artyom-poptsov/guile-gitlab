(define-module (gitlab api project)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:use-module (gitlab session)
  #:use-module (gitlab api common)
  #:export (gitlab-api-projects-get))



(define* (gitlab-api-projects-get session
                                  #:key
                                  (id                #f)
                                  (limit             #f)
                                  (owned?            'undefined))
  (let ((query
         (make-sieved-list
          (cons-or-null 'owned owned?))))
    (if id
        (client-get (gitlab-session-client session)
                    (format #f "/api/v4/projects/~a" id)
                    #:query query)
        (api-get session
                 "/api/v4/projects"
                 #:limit limit
                 #:query query))))

;;; project.scm ends here.

