(define-module (gitlab)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:export (<gitlab>
            gitlab-token
            gitlab-client
            gitlab-request-users
            gitlab-request-groups
            gitlab-delete-user))

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
                                 #:debug? #t
                                 #:token  token
                                 #:server (string->uri endpoint)))))


(define* (gitlab-request-users gitlab
                               #:key
                               (id                #f)
                               (limit             #f)
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
                               (search            'undefined)
                               (without-projects? 'undefined))
  (let ((max-page-size 100)
        (query
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
          (cons-or-null 'search search)
          (cons-or-null 'without_projects without-projects?))))
    (if id
        (client-get (gitlab-client gitlab)
                    (format #f "/api/v4/users/~a" id)
                    #:query query)
        (let ((get (lambda (page page-size)
                     (client-get (gitlab-client gitlab)
                                 "/api/v4/users/"
                                 #:query (cons (cons 'per_page (number->string page-size))
                                               (cons (cons 'page (number->string page))
                                                     query))))))
          (if limit
              (let ((first-page (get 1 limit)))
                (if (>= (vector-length first-page) limit)
                    first-page
                    (let loop ((data   (get 2 (- limit max-page-size)))
                               (result (vector->list first-page))
                               (page   2))
                      (format (current-error-port) "PAGE: ~a~%" page)
                      (cond
                       ((zero? (vector-length data))
                        (list->vector result))
                       ((>= (+ (vector-length data)
                               (length result))
                            limit)
                        (list->vector (append result
                                              (vector->list data))))
                       (else
                        (loop (get (+ page 1)
                                   (- limit (* max-page-size page)))
                              (append result (vector->list data))
                              (+ page 1)))))))
              (let loop ((data   (get 1 max-page-size))
                         (result '())
                         (page   1))
                (if (zero? (vector-length data))
                    (list->vector result)
                    (loop (get (+ page 1) max-page-size)
                          (append result (vector->list data))
                          (+ page 1)))))))))

(define* (gitlab-delete-user gitlab
                             id
                             #:key
                             (hard-delete?   'undefined))
  (let ((query
         (make-sieved-list
          (cons-or-null 'hard_delete hard-delete?))))
    (client-delete (gitlab-client gitlab)
                   (format #f "/api/v4/users/~a" id)
                   #:query query)))

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
