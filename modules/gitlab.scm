(define-module (gitlab)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:export (<gitlab>
            gitlab-token
            gitlab-client
            gitlab-request-users))

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
   #:getter gitlab-client))


(define-method (initialize (gitlab <gitlab>) initargs)
  (next-method)
  (let ((token    (constructor-argument #:token initargs))
        (endpoint (constructor-argument #:endpoint initargs)))
    (gitlab-client-set! gitlab (make <client>
                                 #:debug? #f
                                 #:token  token
                                 #:server (string->uri endpoint)))))


(define* (gitlab-request-users gitlab
                               #:key
                               (id                #f)
                               (username          'undefined)
                               (active?           'undefined)
                               (blocked?          'undefined)
                               (external?         'undefined)
                               (exclude-internal? 'undefined)
                               (exclude-external? 'undefined)
                               (order-by          'undefined)
                               (sort              'undefined)
                               (two-factor        'undefined)
                               (admins            'undefined)
                               (without-projects? 'undefined))
  (let ((query
         (make-sieved-list
           (cons-or-null 'username username)
           (cons-or-null 'active active?)
           (cons-or-null 'blocked blocked?)
           (cons-or-null 'external external?)
           (cons-or-null 'exclude_external exclude-external?)
           (cons-or-null 'exclude_internal exclude-internal?)
           (cons-or-null 'order_by order-by)
           (cons-or-null 'sort sort)
           (cons-or-null 'two_factor two-factor)
           (cons-or-null 'admins admins)
           (cons-or-null 'without_projects without-projects?))))
    (if id
        (client-get (gitlab-client gitlab)
                    (format #f "/api/v4/users/~a" id)
                    #:query query)
        (client-get (gitlab-client gitlab)
                    "/api/v4/users/"
                    #:query query))))


;;; gitlab.scm ends here.
