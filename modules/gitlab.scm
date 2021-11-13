(define-module (gitlab)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:use-module (gitlab session)
  #:use-module (gitlab api user)
  #:use-module (gitlab api group)
  #:re-export (<session>
               gitlab-session-token
               gitlab-session-client
               gitlab-session-debug-mode?
               gitlab-api-users-get
               gitlab-api-users-delete
               gitlab-api-groups-get))

;;; gitlab.scm ends here.
