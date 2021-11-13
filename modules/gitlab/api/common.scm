(define-module (gitlab api common)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:use-module (gitlab session)
  #:export (api-get))


(define* (api-get session
                  resource
                  #:key
                  (query         '())
                  (limit         #f)
                  (max-page-size 100))
  (let ((get (lambda (page page-size)
               (client-get (gitlab-session-client session)
                           resource
                           #:query (cons (cons 'per_page
                                               (number->string page-size))
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
                    (+ page 1)))))))

;;; common.scm
