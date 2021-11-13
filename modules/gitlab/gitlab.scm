(define-module (gitlab gitlab)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:export (<gitlab>
            gitlab-token
            gitlab-client
            gitlab-debug-mode?))


(define-class <gitlab> ()
  ;; GitLab authentication token.
  ;;
  ;; <string>
  (token
   #:init-keyword #:token
   #:getter       gitlab-token)

  ;; GitLab <client> instance.
  ;;
  ;; <client>
  (client
   #:setter gitlab-client-set!
   #:getter gitlab-client)

  ;; <boolean>
  (debug-mode?
   #:init-value   #f
   #:init-keyword #:debug-mode?
   #:getter       gitlab-debug-mode?))

(define-method (initialize (gitlab <gitlab>) initargs)
  (next-method)
  (let ((token       (constructor-argument #:token initargs))
        (endpoint    (constructor-argument #:endpoint initargs))
        (debug-mode? (constructor-argument #:debug-mode? initargs)))
    (gitlab-client-set! gitlab (make <client>
                                 #:debug? debug-mode?
                                 #:token  token
                                 #:server (string->uri endpoint)))))

;;; gitlab.scm ends here.

