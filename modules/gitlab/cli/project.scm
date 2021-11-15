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
Usage: ~a project <sub-command> [arguments]

Sub-commands:
  list, ls    List projects in various ways.
  help, h     Print this message.
"
          program-name))

(define (print-list-help program-name)
  (format #t "\
Usage: ~a project list [arguments]
       ~a project ls   [arguments]

Options:
  --id <id>
              ID of a project that should be requested.
  --limit <limit>
              Limit the number of projects that will be requested
              from the server.  By default, the command will fetch
              all the projects that satisfy the requirements set by
              other options.
  --print, -p <list-of-parameters>
              Print only the specified parameters from this
              comma-separated list.
              Value example: name,email
  --format, f Output format.
              Allowed values:
                  \"scheme\" (default), \"csv\", \"human-readable\"
  --owned? <boolean>
              Print only projects that are owned by the current
              user (the owner of the token)
              Allowed values: true, false
"
          program-name
          program-name))



(define %option-spec
  '((help         (single-char #\h) (value #f))
    (server       (single-char #\s) (value #t))
    (token        (single-char #\t) (value #t))
    (limit        (single-char #\l) (value #t))
    (print        (single-char #\p) (value #t))
    (format       (single-char #\f) (value #t))
    (id                             (value #t))
    (owned?                         (value #t))))

(define (gitlab-cli-project/list program-name args)
  (let* ((options (getopt-long (cons program-name args) %option-spec))
         (help-needed? (option-ref options 'help      #f))
         ;; Required parameters.
         (server       (option-ref options 'server    #f))
         (token        (option-ref options 'token     #f))
         ;; Optional parameters.
         (id           (option-ref options 'id        #f))
         (limit        (option-ref options 'limit     #f))
         (fields       (option-ref options 'print     #f))
         (print-format (string->symbol (option-ref options 'format    "scheme")))
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
          (print result fields #:format print-format)
          (print-many (vector->list result) fields #:format print-format))
      (newline))))


(define %commands
  `((("list" "ls")        ,gitlab-cli-project/list)
    (("help" "h")         ,(lambda (program-name args)
                             (print-help program-name)
                             (exit 0)))))

(define (gitlab-cli-project program-name args)
  (when (zero? (length args))
    (print-help program-name)
    (exit 0))
  (let* ((sub-command (car args))
         (handler     (command-match sub-command %commands)))
    (if handler
        (handler program-name (cdr args))
        (print-help program-name))))

;;; group.scm ends here.
