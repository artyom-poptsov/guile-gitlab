(define-module (gitlab api user)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:use-module (gitlab session)
  #:export (gitlab-api-users-get
            gitlab-api-users-delete))



(define* (gitlab-api-users-get session
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
        (client-get (gitlab-session-client session)
                    (format #f "/api/v4/users/~a" id)
                    #:query query)
        (let ((get (lambda (page page-size)
                     (client-get (gitlab-session-client session)
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
                      (when (gitlab-session-debug-mode? session)
                        (format (current-error-port) "PAGE: ~a~%" page))
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

(define* (gitlab-api-users-delete session
                                  id
                                  #:key
                                  (hard-delete?   'undefined))
  (let ((query
         (make-sieved-list
          (cons-or-null 'hard_delete hard-delete?))))
    (client-delete (gitlab-session-client session)
                   (format #f "/api/v4/users/~a" id)
                   #:query query)))

;;; user.scm ends here.

