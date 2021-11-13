(define-module (gitlab cli project)
  #:use-module (oop goops)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (web uri)
  #:use-module (gitlab)
  #:use-module (gitlab cli common)
  #:export (gitlab-cli-project))



(define (print-help program-name)
  (format #t "\
Usage: ~a project [arguments]

Options:
  --id <id>
              ID of a project that should be requested.
  --limit <limit>
              Limit the number of projects that will be requested
              from the server.  By default, the command will fetch
              all the projects that satisfy the requirements set by
              other options.
  --owned? <boolean>
              Print only projects that are owned by the current
              user (the owner of the token)
              Allowed values: true, false
"
          program-name))



(define %option-spec
  '((help         (single-char #\h) (value #f))
    (server       (single-char #\s) (value #t))
    (token        (single-char #\t) (value #t))
    (limit        (single-char #\l) (value #t))
    (id                             (value #t))
    (owned?                         (value #t))))

(define (gitlab-cli-project program-name args)
  (let* ((options (getopt-long (cons program-name args) %option-spec))
         (help-needed? (option-ref options 'help      #f))
         ;; Required parameters.
         (server       (option-ref options 'server    #f))
         (token        (option-ref options 'token     #f))
         ;; Optional parameters.
         (id           (option-ref options 'id        #f))
         (limit        (option-ref options 'limit     #f))
         (owned?       (option-ref options 'owned?    'undefined)))

    (when (or help-needed? (< (length args) 2))
      (print-help program-name)
      (exit 0))

    (unless server
      (error "'--server' option must be specified" args))

    (unless token
      (error "'--token' option must be specified" args))

    (let* ((session (make <session>
                      #:endpoint server
                      #:token    token))
           (result (gitlab-api-projects-get session
                                            #:id        id
                                            #:limit     (and limit
                                                             (string->number limit))
                                            #:owned?    (if (equal? owned? 'undefined)
                                                            'undefined
                                                            (string->boolean owned?)))))
      (if id
          (print result #f)
          (pretty-print result))
      (newline))))

;;; group.scm ends here.
