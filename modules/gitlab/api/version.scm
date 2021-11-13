(define-module (gitlab api version)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:use-module (gitlab session)
  #:export (gitlab-api-version-get))



(define (gitlab-api-version-get session)
  (client-get (gitlab-session-client session) "/api/v4/version"))

;;; version.scm ends here.
