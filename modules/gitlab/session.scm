(define-module (gitlab session)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:export (<session>
            gitlab-session-token
            gitlab-session-client
            gitlab-session-debug-mode?))


(define-class <session> ()
  ;; GitLab authentication token.
  ;;
  ;; <string>
  (token
   #:init-keyword #:token
   #:getter       gitlab-session-token)

  ;; GitLab <client> instance.
  ;;
  ;; <client>
  (client
   #:setter gitlab-session-client-set!
   #:getter gitlab-session-client)

  ;; <boolean>
  (debug-mode?
   #:init-value   #f
   #:init-keyword #:debug-mode?
   #:getter       gitlab-session-debug-mode?))

(define-method (initialize (session <session>) initargs)
  (next-method)
  (let ((token       (constructor-argument #:token initargs))
        (endpoint    (constructor-argument #:endpoint initargs))
        (debug-mode? (constructor-argument #:debug-mode? initargs)))
    (gitlab-session-client-set! session (make <client>
                                          #:debug? debug-mode?
                                          #:token  token
                                          #:server (string->uri endpoint)))))

;;; session.scm ends here.

