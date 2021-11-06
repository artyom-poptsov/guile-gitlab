(define-module (gitlab cli group)
  #:use-module (oop goops)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (web uri)
  #:use-module (gitlab)
  #:use-module (gitlab cli common)
  #:export (gitlab-cli-group))



(define (print-group-help program-name)
  (format #t "\
Usage: ~a group [arguments]

Options:
  --id <id>
              ID of a group that should be requested.
  --owned? <boolean>
              Print only groups that are owned by the current
              user (the owner of the token)
              Allowed values: true, false
"
          program-name))



(define %group-option-spec
  '((help   (single-char #\h) (value #f))
    (server (single-char #\s) (value #t))
    (token  (single-char #\t) (value #t))
    (id                       (value #t))
    (owned?                   (value #t))))

(define (gitlab-cli-group program-name args)
  (let* ((options (getopt-long args %group-option-spec))
         (help-needed? (option-ref options 'help      #f))
         ;; Required parameters.
         (server       (option-ref options 'server    #f))
         (token        (option-ref options 'token     #f))
         ;; Optional parameters.
         (id           (option-ref options 'id        #f))
         (owned?       (option-ref options 'owned?    'undefined)))

    (when (or help-needed? (< (length args) 2))
      (print-group-help program-name)
      (exit 0))

    (unless token
      (error "GitLab token is not provided."))

    (unless server
      (error "GitLab server URL is not provided."))

    (let* ((gitlab (make <gitlab>
                     #:endpoint server
                     #:token    token))
           (result (gitlab-request-groups gitlab
                                          #:id        id
                                          #:owned?    (if (equal? owned? 'undefined)
                                                          'undefined
                                                          (string->boolean owned?)))))
      (if id
          (print result #f)
          (pretty-print result))
      (newline))))

;;; group.scm ends here.
